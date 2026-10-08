      *>================================================================
      *> PROGRAM-ID : TPOGAP
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : UNIT TESTS FOR THE FOUR PURCHASING RULES THAT
      *>              HAD NO TEST OF THEIR OWN - APPROVED SOURCE
      *>              SELECTION (PO-002), COMPLETE ORDER CAPTURE AND
      *>              FREE TEXT CODING (PO-003), COPIES AND BLANKET
      *>              CALL-OFFS (PO-021) AND SUPPLIER DUE DILIGENCE
      *>              (PO-026).  THE SUITE OWNS ZZ- PREFIXED FIXTURES,
      *>              DELETES THEM BEFORE AND AFTER EVERY RUN AND
      *>              SKIPS ENTIRELY WHEN THE DATABASE IS DOWN, SO IT
      *>              MAY BE RUN AS OFTEN AS YOU LIKE.
      *> CALLS      : SOURCSEL, POORDER, POAMEND, POSUPCHK, SQLIO
      *>----------------------------------------------------------------
      *> 2026-10-07 STERP ORIGINAL
      *>================================================================
      *> REQ: PO-002, PO-003, PO-021, PO-026
      *> AC: AC-050, AC-058, AC-114
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TPOGAP.
       AUTHOR. STERP.
       DATE-WRITTEN. 07/10/2026.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY SQLIOCA.
       COPY SOURCSELCA REPLACING ==DFHCOMMAREA== BY ==WS-SS==
                       LEADING ==CA-== BY ==SC-==.
       COPY POORDERCA REPLACING ==DFHCOMMAREA== BY ==WS-OR==
                      LEADING ==CA-== BY ==OC-==.
       COPY POSUPCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-DD==
                       LEADING ==CA-== BY ==DC-==.
       01  WS-ANS-ROW.
           05  WS-ANS                  PIC X(100).
       01  WS-SWITCH.
           05  WS-DB-OK                PIC X VALUE "N".
       01  WS-CORR-ID                  PIC X(36) VALUE SPACES.
       01  WS-WORK.
           05  WS-CORR-SEQ             PIC 9(4) VALUE 0.
           05  WS-CORR-ED              PIC Z(3)9.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-TYPE                 PIC X(20) VALUE SPACES.
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-START.
           MOVE "po purchasing coverage gaps" TO TK-SUITE
           PERFORM TK-BEGIN
           MOVE 0 TO WS-CORR-SEQ
           MOVE 0 TO WS-I
           MOVE SPACES TO WS-CORR-ID
           MOVE "N" TO WS-DB-OK
           PERFORM 0100-CONNECT THRU 0100-EXIT
           IF WS-DB-OK = "Y"
               PERFORM 0200-CLEAN THRU 0200-EXIT
               PERFORM 0300-FIXTURES THRU 0300-EXIT
               PERFORM 1000-SRC-APPROVED THRU 1000-EXIT
               PERFORM 1100-SRC-UNAPPROVED THRU 1100-EXIT
               PERFORM 1200-SRC-RESTRICTED THRU 1200-EXIT
               PERFORM 1300-SRC-COMPARE THRU 1300-EXIT
               PERFORM 1400-SRC-SELECT THRU 1400-EXIT
               PERFORM 2000-ORD-CAPTURE THRU 2000-EXIT
               PERFORM 2100-ORD-FREETEXT THRU 2100-EXIT
               PERFORM 2200-ORD-GOVERNED THRU 2200-EXIT
               PERFORM 2300-ORD-SCHEDULE THRU 2300-EXIT
               PERFORM 3000-BLK-CALLOFF THRU 3000-EXIT
               PERFORM 3100-ORD-COPY THRU 3100-EXIT
               PERFORM 4000-DIL-NOTHING THRU 4000-EXIT
               PERFORM 4100-DIL-BLOCKING THRU 4100-EXIT
               PERFORM 4200-DIL-EXPIRED THRU 4200-EXIT
               PERFORM 4300-ORD-BLOCKED THRU 4300-EXIT
               PERFORM 0200-CLEAN THRU 0200-EXIT
               PERFORM 9900-DISCONNECT THRU 9900-EXIT
           ELSE
               MOVE "no database connection" TO TK-DETAIL
               MOVE "po purchasing gaps need the STERP database"
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
           CALL "SQLIO" USING SQLIO-CA WS-ANS-ROW
           IF SQLIO-SQLCODE = 0
               MOVE "Y" TO WS-DB-OK
           END-IF.
       0100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> EVERY FIXTURE CARRIES A ZZ KEY OF ITS OWN, SO A FAILED RUN
      *> CANNOT LEAVE A SOURCE, AN ORDER OR A PIECE OF DUE DILIGENCE
      *> EVIDENCE BEHIND FOR THE NEXT ONE.
      *>----------------------------------------------------------------
       0200-CLEAN SECTION.
       0200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM po.PurchaseSchedule WHERE "
                  "OrderNumber LIKE 'ZZ-TEST-GAP%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM po.PurchaseOrderLine WHERE "
                  "OrderNumber LIKE 'ZZ-TEST-GAP%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM po.OrderAmendment WHERE "
                  "OrderNumber LIKE 'ZZ-TEST-GAP%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM po.Commitment WHERE OrderNumber "
                  "LIKE 'ZZ-TEST-GAP%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM po.PurchaseOrder WHERE OrderNumber "
                  "LIKE 'ZZ-TEST-GAP%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE q FROM po.SourcingQuote q JOIN "
                  "po.SourcingEvent e ON e.SourcingEventId = "
                  "q.SourcingEventId WHERE e.SourcingRef LIKE "
                  "'ZZ-TEST-GAP%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM po.SourcingEvent WHERE SourcingRef "
                  "LIKE 'ZZ-TEST-GAP%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM po.SupplierSource WHERE ItemCode "
                  "LIKE 'ZZ-TEST-GAP%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM po.SupplierDiligence WHERE "
                  "SupplierCode LIKE 'ZZTESTGAP%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM po.BlanketBalance WHERE "
                  "BlanketOrderNumber LIKE 'ZZ-TEST-GAP%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM core.Approval WHERE DocumentRef "
                  "LIKE 'ZZ-TEST-GAP%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM core.ExceptionItem WHERE "
                  "DocumentRef LIKE 'ZZ-TEST-GAP%' OR DocumentRef "
                  "LIKE 'ZZTESTGAP%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM ap.Supplier WHERE SupplierCode "
                  "LIKE 'ZZTESTGAP%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT.
       0200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE APPROVED SOURCE REGISTER OF PO-002 IN MINIATURE - ONE
      *> APPROVED SOURCE WITH A CONTRACT, A LEAD TIME AND A MINIMUM
      *> ORDER QUANTITY, ONE CONDITIONAL SOURCE WITH A COMMERCIAL
      *> RESTRICTION, ONE SUSPENDED, ONE UNAPPROVED AND ONE WHOSE
      *> APPROVAL HAS RUN OUT OF DATE.
      *>----------------------------------------------------------------
       0300-FIXTURES SECTION.
       0300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ap.Supplier (CompanyCode, SupplierCode, "
                  "LegalName, CurrencyCode, TermsCode, Status, "
                  "CreatedBy) VALUES ('STC', 'ZZTESTGAPA', "
                  "'ZZ Test Approved Source Ltd', 'GBP', 'N30', "
                  "'ACTIVE', 'TEST'), ('STC', 'ZZTESTGAPB', "
                  "'ZZ Test Blocked Supplier Ltd', 'GBP', 'N30', "
                  "'ACTIVE', 'TEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.SupplierSource (CompanyCode, ItemCode"
                  ", SupplierCode, SupplierPartNumber, "
                  "ApprovalStatus, ContractRef, LeadTimeDays, "
                  "MinOrderQty, PackQty, CurrencyCode, "
                  "CommercialRestriction, EffectiveFrom, "
                  "EffectiveTo, CreatedBy) VALUES ('STC', "
                  "'ZZ-TEST-GAP-ITEM', 'ZZTESTGAPA', 'ZZ-PART-A', "
                  "'APPROVED', 'ZZ-TEST-GAP-CONTRACT', 14, 10, 5, "
                  "'GBP', NULL, '2026-01-01', NULL, 'TEST'), "
                  "('STC', 'ZZ-TEST-GAP-ITEM', 'ZZTESTGAPU', "
                  "'ZZ-PART-U', 'UNAPPROVED', NULL, 30, 0, 1, "
                  "'GBP', NULL, '2026-01-01', NULL, 'TEST'), "
                  "('STC', 'ZZ-TEST-GAP-ITEM', 'ZZTESTGAPS', "
                  "'ZZ-PART-S', 'SUSPENDED', NULL, 21, 0, 1, "
                  "'GBP', NULL, '2026-01-01', NULL, 'TEST'), "
                  "('STC', 'ZZ-TEST-GAP-ITEM', 'ZZTESTGAPC', "
                  "'ZZ-PART-C', 'CONDITION', NULL, 35, 0, 1, "
                  "'GBP', 'Tooling release needed from the owner', "
                  "'2026-01-01', NULL, 'TEST'), ('STC', "
                  "'ZZ-TEST-GAP-ITEM', 'ZZTESTGAPX', 'ZZ-PART-X', "
                  "'APPROVED', NULL, 28, 0, 1, 'GBP', NULL, "
                  "'2026-01-01', '2026-06-30', 'TEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.BlanketBalance (CompanyCode, "
                  "BlanketOrderNumber, LineNum, ItemCode, "
                  "Description, AgreedQty, CalledOffQty, "
                  "AgreedValue, CalledOffValue, CurrencyCode, "
                  "ValidFrom, ValidTo, Status, ModifiedBy) VALUES "
                  "('STC', 'ZZ-TEST-GAP-BLK', 1, "
                  "'ZZ-TEST-GAP-ITEM', 'ZZ blanket agreement', 100,"
                  " 0, 10000.00, 0, 'GBP', '2026-01-01', "
                  "'2026-12-31', 'OPEN', 'TEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT.
       0300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0900-EXEC SECTION.
       0900-START.
           MOVE "EXEC" TO SQLIO-FUNC
           MOVE 7 TO SQLIO-CURSOR
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ANS-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ANS-ROW.
       0900-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> ONE COLUMN, ONE ROW, STRAIGHT INTO WS-ANS.  THE TEST READS
      *> THE DATABASE BACK RATHER THAN BELIEVING THE COMMAREA.
      *>----------------------------------------------------------------
       0950-READ-ANS SECTION.
       0950-START.
           MOVE "SNGL" TO SQLIO-FUNC
           MOVE 7 TO SQLIO-CURSOR
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 100 TO SQLIO-COL-WIDTH(1)
           MOVE SPACES TO WS-ANS
           CALL "SQLIO" USING SQLIO-CA WS-ANS-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ANS-ROW.
       0950-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0960-NEXT-CORR SECTION.
       0960-START.
           ADD 1 TO WS-CORR-SEQ
           MOVE WS-CORR-SEQ TO WS-CORR-ED
           MOVE SPACES TO WS-CORR-ID
           STRING "ZZ-TEST-GAP-" FUNCTION TRIM(WS-CORR-ED)
               DELIMITED BY SIZE INTO WS-CORR-ID.
       0960-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0970-CALL-SRC SECTION.
       0970-START.
           MOVE "STC" TO SC-COMPANY
           MOVE "RDG" TO SC-SITE
           MOVE "ZZTEST" TO SC-USER
           MOVE 20261006 TO SC-BUSINESS-DATE
           PERFORM 0960-NEXT-CORR THRU 0960-EXIT
           MOVE WS-CORR-ID TO SC-CORRELATION-ID
           CALL "SOURCSEL" USING WS-SS.
       0970-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0980-CALL-ORD SECTION.
       0980-START.
           MOVE "STC" TO OC-COMPANY
           MOVE "RDG" TO OC-SITE
           MOVE "ZZTEST" TO OC-USER
           MOVE 20261006 TO OC-BUSINESS-DATE
           PERFORM 0960-NEXT-CORR THRU 0960-EXIT
           MOVE WS-CORR-ID TO OC-CORRELATION-ID
           CALL "POORDER" USING WS-OR.
       0980-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0990-CALL-DIL SECTION.
       0990-START.
           MOVE "STC" TO DC-COMPANY
           MOVE "RDG" TO DC-SITE
           MOVE "ZZTEST" TO DC-USER
           MOVE 20261006 TO DC-BUSINESS-DATE
           PERFORM 0960-NEXT-CORR THRU 0960-EXIT
           MOVE WS-CORR-ID TO DC-CORRELATION-ID
           CALL "POSUPCHK" USING WS-DD.
       0990-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-002.  AN APPROVED SOURCE ANSWERS WITH THE COMMERCIAL
      *> FACTS THE BUYER COMPARES ON - THE SUPPLIER'S OWN PART
      *> NUMBER, THE CONTRACT IT IS APPROVED UNDER, THE LEAD TIME
      *> AND THE MINIMUM ORDER QUANTITY.
      *> REQ: PO-002   AC: AC-050
      *>----------------------------------------------------------------
       1000-SRC-APPROVED SECTION.
       1000-START.
           INITIALIZE WS-SS
           MOVE "CHECK" TO SC-FUNCTION
           MOVE "ZZ-TEST-GAP-ITEM" TO SS-ITEM-CODE
           MOVE "ZZTESTGAPA" TO SS-SUPPLIER
           MOVE 50 TO SS-QUANTITY
           PERFORM 0970-CALL-SRC THRU 0970-EXIT
           MOVE "PO-002 an approved source is accepted" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-002 the approval status is reported" TO TK-NAME
           MOVE "APPROVED" TO TK-EXPECTED-X
           MOVE SS-APPROVAL-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-002 the supplier part number is reported"
             TO TK-NAME
           MOVE "ZZ-PART-A" TO TK-EXPECTED-X
           MOVE SS-SUPPLIER-PART TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-002 the approved contract reference is given"
             TO TK-NAME
           MOVE "ZZ-TEST-GAP-CONTRACT" TO TK-EXPECTED-X
           MOVE SS-CONTRACT-REF TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-002 the lead time of the source is 14 days"
             TO TK-NAME
           MOVE 14 TO TK-EXPECTED-N
           MOVE SS-LEAD-TIME-DAYS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-002 an approved source needs no authority"
             TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE SS-NEEDS-AUTH TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-SS
           MOVE "CHECK" TO SC-FUNCTION
           MOVE "ZZ-TEST-GAP-ITEM" TO SS-ITEM-CODE
           MOVE "ZZTESTGAPA" TO SS-SUPPLIER
           MOVE 5 TO SS-QUANTITY
           PERFORM 0970-CALL-SRC THRU 0970-EXIT
           MOVE "PO-002 ordering below the supplier minimum warns"
             TO TK-NAME
           MOVE "SRC0011" TO TK-EXPECTED-X
           MOVE SC-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-002 the minimum order quantity is 10" TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE SS-MIN-ORDER-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-002.  AN UNAPPROVED SOURCE IS NOT FORBIDDEN, IT IS HELD
      *> FOR EXPLICIT AUTHORISATION; A NAMED AUTHORISER AND A REASON
      *> RELEASE IT AND NOTHING ELSE DOES.  APPROVAL ALSO EXPIRES.
      *> REQ: PO-002   AC: AC-050
      *>----------------------------------------------------------------
       1100-SRC-UNAPPROVED SECTION.
       1100-START.
           INITIALIZE WS-SS
           MOVE "CHECK" TO SC-FUNCTION
           MOVE "ZZ-TEST-GAP-ITEM" TO SS-ITEM-CODE
           MOVE "ZZTESTGAPU" TO SS-SUPPLIER
           MOVE 50 TO SS-QUANTITY
           PERFORM 0970-CALL-SRC THRU 0970-EXIT
           MOVE "PO-002 an unapproved source is held for authority"
             TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE SC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-002 the unapproved source reason code is given"
             TO TK-NAME
           MOVE "SRC0005" TO TK-EXPECTED-X
           MOVE SC-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-002 the unapproved source needs authorisation"
             TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SS-NEEDS-AUTH TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-SS
           MOVE "CHECK" TO SC-FUNCTION
           MOVE "ZZ-TEST-GAP-ITEM" TO SS-ITEM-CODE
           MOVE "ZZTESTGAPU" TO SS-SUPPLIER
           MOVE 50 TO SS-QUANTITY
           MOVE "PURCHMGR" TO SS-AUTHORISED-BY
           MOVE "Sole source for a breakdown repair" TO
                SS-SELECT-REASON
           PERFORM 0970-CALL-SRC THRU 0970-EXIT
           MOVE "PO-002 an authorised unapproved source may be used"
             TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE SC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-002 an authorised source needs no authority"
             TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE SS-NEEDS-AUTH TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-SS
           MOVE "CHECK" TO SC-FUNCTION
           MOVE "ZZ-TEST-GAP-ITEM" TO SS-ITEM-CODE
           MOVE "ZZTESTGAPX" TO SS-SUPPLIER
           MOVE 50 TO SS-QUANTITY
           PERFORM 0970-CALL-SRC THRU 0970-EXIT
           MOVE "PO-002 an approval that has expired needs authority"
             TO TK-NAME
           MOVE "SRC0004" TO TK-EXPECTED-X
           MOVE SC-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-002 an expired approval is held, not accepted"
             TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE SC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-002.  COMMERCIAL RESTRICTIONS ARE PART OF THE DECISION -
      *> A SUSPENDED SOURCE IS REFUSED OUTRIGHT, A CONDITIONAL ONE
      *> IS ALLOWED ONLY WITH ITS RESTRICTION IN FRONT OF THE BUYER.
      *> REQ: PO-002
      *>----------------------------------------------------------------
       1200-SRC-RESTRICTED SECTION.
       1200-START.
           INITIALIZE WS-SS
           MOVE "CHECK" TO SC-FUNCTION
           MOVE "ZZ-TEST-GAP-ITEM" TO SS-ITEM-CODE
           MOVE "ZZTESTGAPS" TO SS-SUPPLIER
           MOVE 50 TO SS-QUANTITY
           PERFORM 0970-CALL-SRC THRU 0970-EXIT
           MOVE "PO-002 a suspended source is refused" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE SC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-002 the suspension reason code is given"
             TO TK-NAME
           MOVE "SRC0003" TO TK-EXPECTED-X
           MOVE SC-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-SS
           MOVE "CHECK" TO SC-FUNCTION
           MOVE "ZZ-TEST-GAP-ITEM" TO SS-ITEM-CODE
           MOVE "ZZTESTGAPC" TO SS-SUPPLIER
           MOVE 50 TO SS-QUANTITY
           PERFORM 0970-CALL-SRC THRU 0970-EXIT
           MOVE "PO-002 a conditional source warns on restriction"
             TO TK-NAME
           MOVE "SRC0010" TO TK-EXPECTED-X
           MOVE SC-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-002 the commercial restriction is shown"
             TO TK-NAME
           MOVE "Tooling release needed from the owner"
             TO TK-EXPECTED-X
           MOVE SS-SRC-RESTRICTION TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-002.  THREE QUOTATIONS ARE COMPARED.  THE CHEAPEST IS
      *> NAMED BUT NOT CHOSEN - 100 OFF AT 9.00 IS 900.00 FROM THE
      *> CONDITIONAL SOURCE, 1,000.00 FROM THE APPROVED ONE AND
      *> 1,200.00 FROM THE UNAPPROVED ONE.
      *> REQ: PO-002, PO-004   AC: AC-050
      *>----------------------------------------------------------------
       1300-SRC-COMPARE SECTION.
       1300-START.
           INITIALIZE WS-SS
           MOVE "OPEN" TO SC-FUNCTION
           MOVE "ZZ-TEST-GAP-SRC" TO SS-SOURCING-REF
           MOVE "ZZ-TEST-GAP-ITEM" TO SS-ITEM-CODE
           MOVE "ZZ gap suite sourcing event" TO SS-DESCRIPTION
           MOVE 100 TO SS-QUANTITY
           MOVE "EA" TO SS-UOM-CODE
           MOVE "ZZTEST" TO SS-BUYER
           MOVE 20261130 TO SS-NEEDED-BY
           PERFORM 0970-CALL-SRC THRU 0970-EXIT
           MOVE "PO-002 a sourcing event is opened for the line"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 1310-QUOTE-C THRU 1310-EXIT
           PERFORM 1320-QUOTE-A THRU 1320-EXIT
           PERFORM 1330-QUOTE-U THRU 1330-EXIT
           INITIALIZE WS-SS
           MOVE "RANK" TO SC-FUNCTION
           MOVE "ZZ-TEST-GAP-SRC" TO SS-SOURCING-REF
           MOVE "ZZTESTGAPA" TO SS-SUPPLIER
           PERFORM 0970-CALL-SRC THRU 0970-EXIT
           MOVE "PO-002 all three quotations are compared" TO TK-NAME
           MOVE 3 TO TK-EXPECTED-N
           MOVE SS-QUOTE-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-002 the cheapest quotation is named" TO TK-NAME
           MOVE "ZZTESTGAPC" TO TK-EXPECTED-X
           MOVE SS-CHEAPEST-SUPP TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-002 the cheapest quotation is 900.00" TO TK-NAME
           MOVE 900.00 TO TK-EXPECTED-N
           MOVE SS-CHEAPEST-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-002 the approved source quotes 1000.00"
             TO TK-NAME
           MOVE 1000.00 TO TK-EXPECTED-N
           MOVE SS-SELECTED-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-002 the premium over the cheapest is 100.00"
             TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE SS-PREMIUM-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-002 the approved source ranks second on price"
             TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           MOVE SS-RANK TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1310-QUOTE-C SECTION.
       1310-START.
           INITIALIZE WS-SS
           MOVE "QUOTE" TO SC-FUNCTION
           MOVE "ZZ-TEST-GAP-SRC" TO SS-SOURCING-REF
           MOVE 1 TO SS-LINE-NUM
           MOVE "ZZTESTGAPC" TO SS-SUPPLIER
           MOVE "ZZ-QUOTE-C" TO SS-QUOTE-REF
           MOVE "GBP" TO SS-CURRENCY
           MOVE 9.00 TO SS-UNIT-PRICE
           MOVE 1 TO SS-PRICE-PER
           MOVE 100 TO SS-QUANTITY
           MOVE 35 TO SS-LEAD-TIME-IN
           MOVE 20270331 TO SS-QUOTE-VALID-TO
           MOVE 70.0000 TO SS-ONTIME-PCT
           MOVE 88.0000 TO SS-QUALITY-PCT
           MOVE "Tooling release needed from the owner"
             TO SS-RESTRICTION
           PERFORM 0970-CALL-SRC THRU 0970-EXIT
           MOVE "PO-002 the cheapest quotation is recorded"
             TO TK-NAME
           MOVE 900.00 TO TK-EXPECTED-N
           MOVE SS-LINE-VALUE-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1310-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1320-QUOTE-A SECTION.
       1320-START.
           INITIALIZE WS-SS
           MOVE "QUOTE" TO SC-FUNCTION
           MOVE "ZZ-TEST-GAP-SRC" TO SS-SOURCING-REF
           MOVE 2 TO SS-LINE-NUM
           MOVE "ZZTESTGAPA" TO SS-SUPPLIER
           MOVE "ZZ-QUOTE-A" TO SS-QUOTE-REF
           MOVE "GBP" TO SS-CURRENCY
           MOVE 10.00 TO SS-UNIT-PRICE
           MOVE 1 TO SS-PRICE-PER
           MOVE 100 TO SS-QUANTITY
           MOVE 14 TO SS-LEAD-TIME-IN
           MOVE 20270331 TO SS-QUOTE-VALID-TO
           MOVE 98.0000 TO SS-ONTIME-PCT
           MOVE 99.5000 TO SS-QUALITY-PCT
           PERFORM 0970-CALL-SRC THRU 0970-EXIT
           MOVE "PO-002 the approved source quotation is recorded"
             TO TK-NAME
           MOVE 1000.00 TO TK-EXPECTED-N
           MOVE SS-LINE-VALUE-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1320-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1330-QUOTE-U SECTION.
       1330-START.
           INITIALIZE WS-SS
           MOVE "QUOTE" TO SC-FUNCTION
           MOVE "ZZ-TEST-GAP-SRC" TO SS-SOURCING-REF
           MOVE 3 TO SS-LINE-NUM
           MOVE "ZZTESTGAPU" TO SS-SUPPLIER
           MOVE "ZZ-QUOTE-U" TO SS-QUOTE-REF
           MOVE "GBP" TO SS-CURRENCY
           MOVE 12.00 TO SS-UNIT-PRICE
           MOVE 1 TO SS-PRICE-PER
           MOVE 100 TO SS-QUANTITY
           MOVE 10 TO SS-LEAD-TIME-IN
           MOVE 20260930 TO SS-QUOTE-VALID-TO
           PERFORM 0970-CALL-SRC THRU 0970-EXIT
           MOVE "PO-002 a quotation past its validity date warns"
             TO TK-NAME
           MOVE "SRC0009" TO TK-EXPECTED-X
           MOVE SC-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1330-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-002.  THE CHEAPEST PRICE IS NOT THE AUTOMATIC WINNER,
      *> BUT PAYING A PREMIUM WITHOUT SAYING WHY IS REFUSED.  WITH A
      *> REASON THE SELECTION STANDS, AND THE REASON AND THE FACT
      *> THAT PRICE DID NOT DECIDE IT ARE BOTH ON FILE.
      *> REQ: PO-002   AC: AC-050
      *>----------------------------------------------------------------
       1400-SRC-SELECT SECTION.
       1400-START.
           INITIALIZE WS-SS
           MOVE "SELECT" TO SC-FUNCTION
           MOVE "ZZ-TEST-GAP-SRC" TO SS-SOURCING-REF
           MOVE "ZZTESTGAPA" TO SS-SUPPLIER
           PERFORM 0970-CALL-SRC THRU 0970-EXIT
           MOVE "PO-002 a dearer choice without a reason is refused"
             TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE SC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-002 the premium needs a justification"
             TO TK-NAME
           MOVE "SRC0007" TO TK-EXPECTED-X
           MOVE SC-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(SelectedSupplierCode, 'NONE') + "
                  "'|' + Status FROM po.SourcingEvent WHERE "
                  "CompanyCode = 'STC' AND SourcingRef = "
                  "'ZZ-TEST-GAP-SRC'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0950-READ-ANS THRU 0950-EXIT
           MOVE "PO-002 a refused selection changes nothing on file"
             TO TK-NAME
           MOVE "NONE|OPEN" TO TK-EXPECTED-X
           MOVE WS-ANS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-SS
           MOVE "SELECT" TO SC-FUNCTION
           MOVE "ZZ-TEST-GAP-SRC" TO SS-SOURCING-REF
           MOVE "ZZTESTGAPA" TO SS-SUPPLIER
           MOVE "Lead time 14 days against 35 and 98% on time"
             TO SS-SELECT-REASON
           PERFORM 0970-CALL-SRC THRU 0970-EXIT
           MOVE "PO-002 a justified premium selection is accepted"
             TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE SC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-002 the selection was not made on price"
             TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE SS-ON-PRICE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT SelectedSupplierCode + '|' + CASE WHEN "
                  "SelectionReason IS NULL THEN 'NOREASON' ELSE "
                  "'REASON' END + '|' + CAST(SelectedOnPrice AS "
                  "varchar(1)) + '|' + Status FROM "
                  "po.SourcingEvent WHERE CompanyCode = 'STC' AND "
                  "SourcingRef = 'ZZ-TEST-GAP-SRC'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0950-READ-ANS THRU 0950-EXIT
           MOVE "PO-002 the chosen source and its reason are kept"
             TO TK-NAME
           MOVE "ZZTESTGAPA|REASON|0|SELECTED" TO TK-EXPECTED-X
           MOVE WS-ANS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(8)) FROM "
                  "po.SourcingQuote q JOIN po.SourcingEvent e ON "
                  "e.SourcingEventId = q.SourcingEventId WHERE "
                  "e.SourcingRef = 'ZZ-TEST-GAP-SRC' AND "
                  "q.IsSelected = 1 AND q.SupplierCode = "
                  "'ZZTESTGAPA'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0950-READ-ANS THRU 0950-EXIT
           MOVE "PO-002 exactly one quotation is marked as chosen"
             TO TK-NAME
           MOVE "1" TO TK-EXPECTED-X
           MOVE WS-ANS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-003.  THE ORDER HEADER CARRIES THE WHOLE COMMERCIAL
      *> AGREEMENT - SUPPLIER, CURRENCY, TERMS AS AGREED WORDING,
      *> THE INCOTERMS 2020 RULE AND NAMED PLACE, THE POINT AT WHICH
      *> OWNERSHIP PASSES, THE CONDITIONS OF PURCHASE VERSION AND
      *> THE DELIVERY ADDRESS.
      *> REQ: PO-003   AC: AC-050
      *>----------------------------------------------------------------
       2000-ORD-CAPTURE SECTION.
       2000-START.
           INITIALIZE WS-OR
           MOVE "CREATE" TO OC-FUNCTION
           MOVE "ZZ-TEST-GAP-PO1" TO OR-ORDER-NUMBER
           MOVE "STANDARD" TO OR-ORDER-TYPE
           MOVE "ZZTESTGAPA" TO OR-SUPPLIER
           MOVE "ZZTEST" TO OR-BUYER
           MOVE 20261006 TO OR-ORDER-DATE
           MOVE "GBP" TO OR-CURRENCY
           MOVE "N30" TO OR-TERMS-CODE
           MOVE "FCA" TO OR-INCOTERMS
           MOVE "Rotterdam" TO OR-NAMED-PLACE
           MOVE "SHIPMENT" TO OR-OWNERSHIP-AT
           MOVE "ZZ Works, Reading, RG1 1AA" TO OR-DELIVERY-ADDR
           MOVE "PO-TC-2024" TO OR-TC-VERSION
           MOVE "ZZ-SUPP-REF-1" TO OR-SUPPLIER-REF
           PERFORM 0980-CALL-ORD THRU 0980-EXIT
           MOVE "PO-003 a complete order header is captured"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE OC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-003 a new order is a draft, not a commitment"
             TO TK-NAME
           MOVE "DRAFT" TO TK-EXPECTED-X
           MOVE OR-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(IncotermsRule, '(none)') + '|' + "
                  "ISNULL(IncotermsNamedPlace, '(none)') + '|' + "
                  "ISNULL(OwnershipPassesAt, '(none)') + '|' + "
                  "ISNULL(TermsAndConditionsVersion, '(none)') + "
                  "'|' + CurrencyCode + '|' + SupplierCode FROM "
                  "po.PurchaseOrder WHERE CompanyCode = 'STC' AND "
                  "OrderNumber = 'ZZ-TEST-GAP-PO1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0950-READ-ANS THRU 0950-EXIT
           MOVE "PO-003 delivery terms and ownership point are kept"
             TO TK-NAME
           MOVE "FCA|Rotterdam|SHIPMENT|PO-TC-2024|GBP|ZZTESTGAPA"
             TO TK-EXPECTED-X
           MOVE WS-ANS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CASE WHEN TermsSnapshot LIKE '%30 days%' "
                  "THEN 'SNAPSHOT' ELSE ISNULL(TermsSnapshot, "
                  "'(none)') END + '|' + "
                  "ISNULL(DeliveryAddressSnapshot, '(none)') FROM "
                  "po.PurchaseOrder WHERE CompanyCode = 'STC' AND "
                  "OrderNumber = 'ZZ-TEST-GAP-PO1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0950-READ-ANS THRU 0950-EXIT
           MOVE "PO-003 agreed terms and destination are snapshot"
             TO TK-NAME
           MOVE "SNAPSHOT|ZZ Works, Reading, RG1 1AA"
             TO TK-EXPECTED-X
           MOVE WS-ANS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-003.  FREE TEXT IS ALLOWED, UNGOVERNED FREE TEXT IS NOT.
      *> A STOCK LINE WITHOUT AN ITEM IS REFUSED, A NON STOCK OR
      *> SERVICE LINE WITHOUT A NOMINAL ACCOUNT IS REFUSED, AND THE
      *> 2,000.00 SERVICE OBLIGATION OF SPECIFICATION 06 SECTION 9
      *> IS ACCEPTED ONLY ONCE IT IS CODED.
      *> REQ: PO-003   AC: AC-050
      *>----------------------------------------------------------------
       2100-ORD-FREETEXT SECTION.
       2100-START.
           INITIALIZE WS-OR
           MOVE "ADDLINE" TO OC-FUNCTION
           MOVE "ZZ-TEST-GAP-PO1" TO OR-ORDER-NUMBER
           MOVE "STOCK" TO OR-LINE-TYPE
           MOVE "ZZ free text that wants to be a stocked item"
             TO OR-DESCRIPTION
           MOVE 5 TO OR-ORDERED-QTY
           MOVE "EA" TO OR-UOM-CODE
           MOVE 12.00 TO OR-UNIT-PRICE
           PERFORM 0980-CALL-ORD THRU 0980-EXIT
           MOVE "PO-003 free text cannot become a stocked line"
             TO TK-NAME
           MOVE "ORD0018" TO TK-EXPECTED-X
           MOVE OC-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-003 the ungoverned stock line is rejected"
             TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE OC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           INITIALIZE WS-OR
           MOVE "ADDLINE" TO OC-FUNCTION
           MOVE "ZZ-TEST-GAP-PO1" TO OR-ORDER-NUMBER
           MOVE "SERVICE" TO OR-LINE-TYPE
           MOVE "ZZ annual boiler inspection service"
             TO OR-DESCRIPTION
           MOVE 1 TO OR-ORDERED-QTY
           MOVE "EA" TO OR-UOM-CODE
           MOVE 2000.00 TO OR-UNIT-PRICE
           PERFORM 0980-CALL-ORD THRU 0980-EXIT
           MOVE "PO-003 an uncoded service line is rejected"
             TO TK-NAME
           MOVE "ORD0005" TO TK-EXPECTED-X
           MOVE OC-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-OR
           MOVE "ADDLINE" TO OC-FUNCTION
           MOVE "ZZ-TEST-GAP-PO1" TO OR-ORDER-NUMBER
           MOVE "SERVICE" TO OR-LINE-TYPE
           MOVE "ZZ annual boiler inspection service"
             TO OR-DESCRIPTION
           MOVE 1 TO OR-ORDERED-QTY
           MOVE "EA" TO OR-UOM-CODE
           MOVE 2000.00 TO OR-UNIT-PRICE
           MOVE "7100" TO OR-ACCOUNT-CODE
           MOVE "CC100" TO OR-COST-CENTRE
           MOVE "S" TO OR-VAT-CODE
           MOVE 20.0000 TO OR-VAT-RATE-PCT
           MOVE "Y" TO OR-RECOVERABLE-VAT
           PERFORM 0980-CALL-ORD THRU 0980-EXIT
           MOVE "PO-003 a coded service line is accepted" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE OC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-003 the service obligation is 2000.00" TO TK-NAME
           MOVE 2000.00 TO TK-EXPECTED-N
           MOVE OR-LINE-NET-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT LineType + '|' + ISNULL(AccountCode, "
                  "'(none)') + '|' + ISNULL(DimCostCentre, "
                  "'(none)') + '|' + CAST(CAST(NetAmount AS "
                  "decimal(18,2)) AS varchar(20)) + '|' + "
                  "ISNULL(ItemCode, 'NOITEM') FROM "
                  "po.PurchaseOrderLine WHERE CompanyCode = 'STC' "
                  "AND OrderNumber = 'ZZ-TEST-GAP-PO1' AND LineNum "
                  "= 1"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0950-READ-ANS THRU 0950-EXIT
           MOVE "PO-003 the service line keeps its nominal coding"
             TO TK-NAME
           MOVE "SERVICE|7100|CC100|2000.00|NOITEM" TO TK-EXPECTED-X
           MOVE WS-ANS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(8)) FROM "
                  "inv.Item WHERE CompanyCode = 'STC' AND "
                  "(ItemCode LIKE 'ZZ-TEST-GAP%' OR Description "
                  "LIKE 'ZZ annual boiler%')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0950-READ-ANS THRU 0950-EXIT
           MOVE "PO-003 no catalogue item is created by the order"
             TO TK-NAME
           MOVE "0" TO TK-EXPECTED-X
           MOVE WS-ANS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-003.  A STOCK LINE MAY ONLY NAME AN ITEM THE ITEM MASTER
      *> ALREADY GOVERNS, SO TYPING A PART NUMBER THAT NOBODY HAS
      *> APPROVED CANNOT BRING A STOCKED ITEM INTO EXISTENCE.
      *> REQ: PO-003   AC: AC-050
      *>----------------------------------------------------------------
       2200-ORD-GOVERNED SECTION.
       2200-START.
           INITIALIZE WS-OR
           MOVE "ADDLINE" TO OC-FUNCTION
           MOVE "ZZ-TEST-GAP-PO1" TO OR-ORDER-NUMBER
           MOVE "STOCK" TO OR-LINE-TYPE
           MOVE "ZZ-TEST-GAP-NOTANITEM" TO OR-ITEM-CODE
           MOVE "ZZ invented part number" TO OR-DESCRIPTION
           MOVE 10 TO OR-ORDERED-QTY
           MOVE "EA" TO OR-UOM-CODE
           MOVE 5.00 TO OR-UNIT-PRICE
           MOVE "7100" TO OR-ACCOUNT-CODE
           PERFORM 0980-CALL-ORD THRU 0980-EXIT
           MOVE "PO-003 an item outside the catalogue is refused"
             TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE OC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-003 the ungoverned item reason code is given"
             TO TK-NAME
           MOVE "ORD0018" TO TK-EXPECTED-X
           MOVE OC-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-OR
           MOVE "ADDLINE" TO OC-FUNCTION
           MOVE "ZZ-TEST-GAP-PO1" TO OR-ORDER-NUMBER
           MOVE "STOCK" TO OR-LINE-TYPE
           MOVE "BAR-AL6082-25MM" TO OR-ITEM-CODE
           MOVE "Aluminium bar 6082 25mm" TO OR-DESCRIPTION
           MOVE 10 TO OR-ORDERED-QTY
           MOVE "EA" TO OR-UOM-CODE
           MOVE 5.00 TO OR-UNIT-PRICE
           MOVE "S" TO OR-VAT-CODE
           MOVE 20.0000 TO OR-VAT-RATE-PCT
           PERFORM 0980-CALL-ORD THRU 0980-EXIT
           MOVE "PO-003 a governed catalogue item is accepted"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE OC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-003 10 at 5.00 is a 50.00 stock line" TO TK-NAME
           MOVE 50.00 TO TK-EXPECTED-N
           MOVE OR-LINE-NET-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT LineType + '|' + ISNULL(ItemCode, "
                  "'NOITEM') + '|' + ISNULL(UomCode, '(none)') + "
                  "'|' + ISNULL(DestinationSiteCode, '(none)') FROM"
                  " po.PurchaseOrderLine WHERE CompanyCode = 'STC' "
                  "AND OrderNumber = 'ZZ-TEST-GAP-PO1' AND LineNum "
                  "= 2"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0950-READ-ANS THRU 0950-EXIT
           MOVE "PO-003 the stock line keeps item, unit and site"
             TO TK-NAME
           MOVE "STOCK|BAR-AL6082-25MM|EA|RDG" TO TK-EXPECTED-X
           MOVE WS-ANS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       2200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-003.  SCHEDULE INFORMATION BELONGS TO THE ORDER TOO, AND
      *> THE SCHEDULES MUST RECONCILE TO THE AUTHORISED QUANTITY.
      *> REQ: PO-003, PO-008
      *>----------------------------------------------------------------
       2300-ORD-SCHEDULE SECTION.
       2300-START.
           INITIALIZE WS-OR
           MOVE "SCHEDULE" TO OC-FUNCTION
           MOVE "ZZ-TEST-GAP-PO1" TO OR-ORDER-NUMBER
           MOVE 2 TO OR-LINE-NUM
           MOVE 1 TO OR-SCHEDULE-NO
           MOVE 6 TO OR-SCH-QTY
           MOVE 20261120 TO OR-SCH-REQ-DATE
           MOVE "RDG" TO OR-SCH-DEST-SITE
           PERFORM 0980-CALL-ORD THRU 0980-EXIT
           MOVE "PO-003 the first delivery schedule is accepted"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE OC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           INITIALIZE WS-OR
           MOVE "SCHEDULE" TO OC-FUNCTION
           MOVE "ZZ-TEST-GAP-PO1" TO OR-ORDER-NUMBER
           MOVE 2 TO OR-LINE-NUM
           MOVE 2 TO OR-SCHEDULE-NO
           MOVE 4 TO OR-SCH-QTY
           MOVE 20261204 TO OR-SCH-REQ-DATE
           MOVE "RDG" TO OR-SCH-DEST-SITE
           PERFORM 0980-CALL-ORD THRU 0980-EXIT
           MOVE "PO-003 the schedules total the ordered 10" TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE OR-SCHEDULED-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           INITIALIZE WS-OR
           MOVE "SCHEDULE" TO OC-FUNCTION
           MOVE "ZZ-TEST-GAP-PO1" TO OR-ORDER-NUMBER
           MOVE 2 TO OR-LINE-NUM
           MOVE 3 TO OR-SCHEDULE-NO
           MOVE 5 TO OR-SCH-QTY
           MOVE 20261218 TO OR-SCH-REQ-DATE
           PERFORM 0980-CALL-ORD THRU 0980-EXIT
           MOVE "PO-003 schedules above the ordered quantity fail"
             TO TK-NAME
           MOVE "ORD0007" TO TK-EXPECTED-X
           MOVE OC-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       2300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-021.  A BLANKET AGREEMENT OF 100 UNITS AND 10,000.00 IS
      *> DRAWN DOWN BY A CALL-OFF OF 60 AT 100.00.  REPEATING THAT
      *> ACCEPTED CALL-OFF CANNOT ISSUE A SECOND OBLIGATION BECAUSE
      *> ONLY 40 REMAIN, AND THE BALANCE IS UNCHANGED BY THE REFUSAL.
      *> REQ: PO-021   AC: AC-058
      *>----------------------------------------------------------------
       3000-BLK-CALLOFF SECTION.
       3000-START.
           PERFORM 3010-ONE-CALLOFF THRU 3010-EXIT
           MOVE "PO-021 a call-off within the blanket is accepted"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE OC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-021 40 of the 100 agreed units remain" TO TK-NAME
           MOVE 40 TO TK-EXPECTED-N
           MOVE OR-BLANKET-QTY-LEFT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(CAST(CalledOffQty AS decimal(19,2)) "
                  "AS varchar(24)) + '|' + "
                  "CAST(CAST(CalledOffValue AS decimal(18,2)) AS "
                  "varchar(24)) + '|' + Status FROM "
                  "po.BlanketBalance WHERE CompanyCode = 'STC' AND "
                  "BlanketOrderNumber = 'ZZ-TEST-GAP-BLK' AND "
                  "LineNum = 1"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0950-READ-ANS THRU 0950-EXIT
           MOVE "PO-021 the call-off draws down quantity and value"
             TO TK-NAME
           MOVE "60.00|6000.00|OPEN" TO TK-EXPECTED-X
           MOVE WS-ANS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 3010-ONE-CALLOFF THRU 3010-EXIT
           MOVE "PO-021 repeating the call-off is refused" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE OC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-021 the blanket balance reason code is given"
             TO TK-NAME
           MOVE "ORD0011" TO TK-EXPECTED-X
           MOVE OC-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0950-READ-ANS THRU 0950-EXIT
           MOVE "PO-021 no duplicate obligation reaches the blanket"
             TO TK-NAME
           MOVE "60.00|6000.00|OPEN" TO TK-EXPECTED-X
           MOVE WS-ANS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-OR
           MOVE "CALLOFF" TO OC-FUNCTION
           MOVE "ZZ-TEST-GAP-BLK" TO OR-BLANKET-NUMBER
           MOVE 1 TO OR-LINE-NUM
           MOVE 40 TO OR-ORDERED-QTY
           MOVE 100.00 TO OR-UNIT-PRICE
           MOVE 1 TO OR-PRICE-PER
           MOVE 20261006 TO OR-ORDER-DATE
           PERFORM 0980-CALL-ORD THRU 0980-EXIT
           MOVE "PO-021 the final call-off empties the blanket"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE OR-BLANKET-QTY-LEFT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0950-READ-ANS THRU 0950-EXIT
           MOVE "PO-021 an exhausted blanket is marked as such"
             TO TK-NAME
           MOVE "100.00|10000.00|EXHAUSTED" TO TK-EXPECTED-X
           MOVE WS-ANS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3010-ONE-CALLOFF SECTION.
       3010-START.
           INITIALIZE WS-OR
           MOVE "CALLOFF" TO OC-FUNCTION
           MOVE "ZZ-TEST-GAP-BLK" TO OR-BLANKET-NUMBER
           MOVE 1 TO OR-LINE-NUM
           MOVE 60 TO OR-ORDERED-QTY
           MOVE 100.00 TO OR-UNIT-PRICE
           MOVE 1 TO OR-PRICE-PER
           MOVE 20261006 TO OR-ORDER-DATE
           PERFORM 0980-CALL-ORD THRU 0980-EXIT.
       3010-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-021.  A COPY KEEPS THE REFERENCE TO WHAT IT WAS COPIED
      *> FROM AND KEEPS NOTHING ELSE - NO APPROVAL, NO ISSUE, NO
      *> RECEIPT, NO INVOICE MATCH AND NO COMMITMENT.
      *> REQ: PO-021   AC: AC-058
      *>----------------------------------------------------------------
       3100-ORD-COPY SECTION.
       3100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE po.PurchaseOrder SET Status = 'ISSUED', "
                  "DeliveryStatus = 'FULLRECEIVED', MatchStatus = "
                  "'FULLINVOICED', ApprovedBy = 'ZZAPPROVER', "
                  "ApprovedAt = SYSDATETIME(), IssuedAt = "
                  "SYSDATETIME(), AcknowledgedAt = SYSDATETIME(), "
                  "CommitmentGBP = 2050.00, ContentHash = 'ZZHASH' "
                  "WHERE CompanyCode = 'STC' AND OrderNumber = "
                  "'ZZ-TEST-GAP-PO1'; UPDATE po.PurchaseOrderLine "
                  "SET ReceivedQty = OrderedQty, AcceptedQty = "
                  "OrderedQty, InvoicedQty = OrderedQty WHERE "
                  "CompanyCode = 'STC' AND OrderNumber = "
                  "'ZZ-TEST-GAP-PO1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           INITIALIZE WS-OR
           MOVE "CREATE" TO OC-FUNCTION
           MOVE "ZZ-TEST-GAP-PO2" TO OR-ORDER-NUMBER
           MOVE "STANDARD" TO OR-ORDER-TYPE
           MOVE "ZZTESTGAPA" TO OR-SUPPLIER
           MOVE "ZZTEST" TO OR-BUYER
           MOVE 20261006 TO OR-ORDER-DATE
           MOVE "GBP" TO OR-CURRENCY
           MOVE "N30" TO OR-TERMS-CODE
           PERFORM 0980-CALL-ORD THRU 0980-EXIT
           INITIALIZE WS-OR
           MOVE "COPY" TO OC-FUNCTION
           MOVE "ZZ-TEST-GAP-PO2" TO OR-ORDER-NUMBER
           MOVE "ZZ-TEST-GAP-PO1" TO OR-COPY-FROM
           PERFORM 0980-CALL-ORD THRU 0980-EXIT
           MOVE "PO-021 an order may be copied to a new draft"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE OC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-021 both live lines are copied" TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           MOVE OR-LINE-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Status + '|' + DeliveryStatus + '|' + "
                  "MatchStatus + '|' + CAST(CAST(CommitmentGBP AS "
                  "decimal(18,2)) AS varchar(20)) + '|' + CASE WHEN"
                  " ApprovedBy IS NULL THEN 'NOAPPR' ELSE 'APPR' "
                  "END + '|' + CASE WHEN IssuedAt IS NULL THEN "
                  "'NOISSUE' ELSE 'ISSUED' END FROM "
                  "po.PurchaseOrder WHERE CompanyCode = 'STC' AND "
                  "OrderNumber = 'ZZ-TEST-GAP-PO2'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0950-READ-ANS THRU 0950-EXIT
           MOVE "PO-021 the copy carries no approval or issue state"
             TO TK-NAME
           MOVE "DRAFT|NOTRECEIVED|UNINVOICED|0.00|NOAPPR|NOISSUE"
             TO TK-EXPECTED-X
           MOVE WS-ANS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(8)) FROM "
                  "po.PurchaseOrderLine WHERE CompanyCode = 'STC' "
                  "AND OrderNumber = 'ZZ-TEST-GAP-PO2' AND "
                  "ReceivedQty = 0 AND AcceptedQty = 0 AND "
                  "InvoicedQty = 0 AND Status = 'OPEN'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0950-READ-ANS THRU 0950-EXIT
           MOVE "PO-021 the copied lines carry no receipt state"
             TO TK-NAME
           MOVE "2" TO TK-EXPECTED-X
           MOVE WS-ANS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(CopiedFromOrderNumber, '(none)') "
                  "FROM po.PurchaseOrder WHERE CompanyCode = 'STC' "
                  "AND OrderNumber = 'ZZ-TEST-GAP-PO2'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0950-READ-ANS THRU 0950-EXIT
           MOVE "PO-021 the copy retains its source order reference"
             TO TK-NAME
           MOVE "ZZ-TEST-GAP-PO1" TO TK-EXPECTED-X
           MOVE WS-ANS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       3100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-026.  ONBOARDING IS NOT COMPLETE UNTIL THE MANDATORY
      *> EVIDENCE OF D-29 IS HELD, AND THE CHECK SAYS EXACTLY WHICH
      *> PIECES ARE MISSING RATHER THAN FAILING SILENTLY.
      *> REQ: PO-026   AC: AC-114
      *>----------------------------------------------------------------
       4000-DIL-NOTHING SECTION.
       4000-START.
           INITIALIZE WS-DD
           MOVE "CHECK" TO DC-FUNCTION
           MOVE "ZZTESTGAPB" TO DD-SUPPLIER
           MOVE 20261006 TO DD-CHECK-DATE
           PERFORM 0990-CALL-DIL THRU 0990-EXIT
           MOVE "PO-026 a supplier with no evidence is not clear"
             TO TK-NAME
           MOVE 16 TO TK-EXPECTED-N
           MOVE DC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-026 no evidence means no order may be issued"
             TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE DD-CAN-ORDER TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-026 the four mandatory checks are listed"
             TO TK-NAME
           MOVE "SANCTIONS, COMPANIESHSE, VATNUMBER, BUSINESSSIZE"
             TO TK-EXPECTED-X
           MOVE DD-MISSING-LIST TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-026.  A SANCTIONS HIT BLOCKS TRADING OUTRIGHT AND IS THE
      *> ONE RESULT THAT NO AUTHORISED EXCEPTION CAN RELEASE.
      *> REQ: PO-026   AC: AC-114
      *>----------------------------------------------------------------
       4100-DIL-BLOCKING SECTION.
       4100-START.
           INITIALIZE WS-DD
           MOVE "RECORD" TO DC-FUNCTION
           MOVE "ZZTESTGAPB" TO DD-SUPPLIER
           MOVE "SANCTIONS" TO DD-EVIDENCE-TYPE
           MOVE "OFSI-DEMO/ZZ-0001" TO DD-EVIDENCE-REF
           MOVE "FAIL" TO DD-RESULT
           MOVE "Y" TO DD-IS-BLOCKING
           MOVE "Y" TO DD-IS-MANDATORY
           MOVE 20261001 TO DD-VALID-FROM
           MOVE 20270930 TO DD-VALID-TO
           MOVE "Name match on the UK sanctions list"
             TO DD-NOTE
           MOVE "ZZSCREEN" TO DD-CHECKED-BY
           PERFORM 0990-CALL-DIL THRU 0990-EXIT
           MOVE "PO-026 a blocking screening result is recorded"
             TO TK-NAME
           MOVE "BLOCKED" TO TK-EXPECTED-X
           MOVE DD-OVERALL TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-DD
           MOVE "CHECK" TO DC-FUNCTION
           MOVE "ZZTESTGAPB" TO DD-SUPPLIER
           MOVE 20261006 TO DD-CHECK-DATE
           PERFORM 0990-CALL-DIL THRU 0990-EXIT
           MOVE "PO-026 a sanctions hit blocks the supplier"
             TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE DC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-026 the blocking reason code is given" TO TK-NAME
           MOVE "DIL0003" TO TK-EXPECTED-X
           MOVE DC-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-026 the blocking check is named" TO TK-NAME
           MOVE "SANCTIONS" TO TK-EXPECTED-X
           MOVE DD-WORST-TYPE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-026 a blocked supplier may not be ordered from"
             TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE DD-CAN-ORDER TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-DD
      *>     "EXCEPTION" IS NINE CHARACTERS AND ARRIVES TRUNCATED IN
      *>     THE EIGHT BYTE CA-FUNCTION; THE LEGACY SPELLING MUST
      *>     STILL REACH THE FUNCTION.
           MOVE "EXCEPTION" TO DC-FUNCTION
           MOVE "ZZTESTGAPB" TO DD-SUPPLIER
           MOVE "SANCTIONS" TO DD-EVIDENCE-TYPE
           MOVE "PURCHMGR" TO DD-EXCEPTION-BY
           MOVE "Urgent spare needed for a breakdown"
             TO DD-EXCEPTION-REASON
           PERFORM 0990-CALL-DIL THRU 0990-EXIT
           MOVE "PO-026 a sanctions block cannot be excepted"
             TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE DC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-026 the unexceptable reason code is given"
             TO TK-NAME
           MOVE "DIL0010" TO TK-EXPECTED-X
           MOVE DC-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       4100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-026.  EXPIRED MANDATORY EVIDENCE HOLDS THE SUPPLIER FOR
      *> AN AUTHORISED EXCEPTION; THE EXCEPTION NEEDS AN APPROVER
      *> AND A REASON, AND ONLY THEN MAY AN ORDER BE ISSUED.
      *> REQ: PO-026   AC: AC-114
      *>----------------------------------------------------------------
       4200-DIL-EXPIRED SECTION.
       4200-START.
           MOVE "SANCTIONS" TO DD-EVIDENCE-TYPE
           PERFORM 4250-RECORD-PASS THRU 4250-EXIT
           MOVE "COMPANIESHSE" TO DD-EVIDENCE-TYPE
           PERFORM 4250-RECORD-PASS THRU 4250-EXIT
           MOVE "VATNUMBER" TO DD-EVIDENCE-TYPE
           PERFORM 4250-RECORD-PASS THRU 4250-EXIT
           MOVE "BUSINESSSIZE" TO DD-EVIDENCE-TYPE
           PERFORM 4250-RECORD-PASS THRU 4250-EXIT
           INITIALIZE WS-DD
           MOVE "RECORD" TO DC-FUNCTION
           MOVE "ZZTESTGAPA" TO DD-SUPPLIER
           MOVE "INSURANCE" TO DD-EVIDENCE-TYPE
           MOVE "ZZ-POLICY-1234" TO DD-EVIDENCE-REF
           MOVE "PASS" TO DD-RESULT
           MOVE "N" TO DD-IS-BLOCKING
           MOVE "Y" TO DD-IS-MANDATORY
           MOVE 20250401 TO DD-VALID-FROM
           MOVE 20260331 TO DD-VALID-TO
           MOVE "ZZSCREEN" TO DD-CHECKED-BY
           PERFORM 0990-CALL-DIL THRU 0990-EXIT
           INITIALIZE WS-DD
           MOVE "CHECK" TO DC-FUNCTION
           MOVE "ZZTESTGAPA" TO DD-SUPPLIER
           MOVE 20261006 TO DD-CHECK-DATE
           PERFORM 0990-CALL-DIL THRU 0990-EXIT
           MOVE "PO-026 evidence out of date holds the supplier"
             TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE DC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-026 the expired evidence reason code is given"
             TO TK-NAME
           MOVE "DIL0004" TO TK-EXPECTED-X
           MOVE DC-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-026 one piece of evidence has expired" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE DD-EXPIRED-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-026 an expired check needs an exception"
             TO TK-NAME
           MOVE "EXCEPTION" TO TK-EXPECTED-X
           MOVE DD-OVERALL TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-DD
           MOVE "EXCEPT" TO DC-FUNCTION
           MOVE "ZZTESTGAPA" TO DD-SUPPLIER
           MOVE "INSURANCE" TO DD-EVIDENCE-TYPE
           PERFORM 0990-CALL-DIL THRU 0990-EXIT
           MOVE "PO-026 an exception needs an approver and a reason"
             TO TK-NAME
           MOVE "DIL0007" TO TK-EXPECTED-X
           MOVE DC-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-DD
           MOVE "EXCEPT" TO DC-FUNCTION
           MOVE "ZZTESTGAPA" TO DD-SUPPLIER
           MOVE "INSURANCE" TO DD-EVIDENCE-TYPE
           MOVE "PURCHMGR" TO DD-EXCEPTION-BY
           MOVE "Renewal certificate due within 14 days"
             TO DD-EXCEPTION-REASON
           PERFORM 0990-CALL-DIL THRU 0990-EXIT
           MOVE "PO-026 an authorised exception is recorded"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE DC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           INITIALIZE WS-DD
           MOVE "CHECK" TO DC-FUNCTION
           MOVE "ZZTESTGAPA" TO DD-SUPPLIER
           MOVE 20261006 TO DD-CHECK-DATE
           PERFORM 0990-CALL-DIL THRU 0990-EXIT
           MOVE "PO-026 the authorised exception releases the order"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE DC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-026 the supplier may now be ordered from"
             TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE DD-CAN-ORDER TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-026 the exception is counted, not forgotten"
             TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE DD-EXCEPTED-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       4200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4250-RECORD-PASS SECTION.
       4250-START.
           MOVE DD-EVIDENCE-TYPE TO WS-TYPE
           INITIALIZE WS-DD
           MOVE "RECORD" TO DC-FUNCTION
           MOVE "ZZTESTGAPA" TO DD-SUPPLIER
           MOVE WS-TYPE TO DD-EVIDENCE-TYPE
           MOVE "ZZ-EVIDENCE-OK" TO DD-EVIDENCE-REF
           MOVE "PASS" TO DD-RESULT
           MOVE "N" TO DD-IS-BLOCKING
           MOVE "Y" TO DD-IS-MANDATORY
           MOVE 20260401 TO DD-VALID-FROM
           MOVE 20270331 TO DD-VALID-TO
           MOVE 20270301 TO DD-NEXT-REVIEW
           MOVE "ZZSCREEN" TO DD-CHECKED-BY
           PERFORM 0990-CALL-DIL THRU 0990-EXIT.
       4250-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-026.  THE RULE THAT MATTERS COMMERCIALLY - A BLOCKED
      *> SUPPLIER CANNOT BE SENT A PURCHASE ORDER, SO ISSUE IS HELD
      *> AND NO COMMITMENT IS WRITTEN.
      *> REQ: PO-026, PO-006   AC: AC-114
      *>----------------------------------------------------------------
       4300-ORD-BLOCKED SECTION.
       4300-START.
           INITIALIZE WS-OR
           MOVE "CREATE" TO OC-FUNCTION
           MOVE "ZZ-TEST-GAP-PO3" TO OR-ORDER-NUMBER
           MOVE "STANDARD" TO OR-ORDER-TYPE
           MOVE "ZZTESTGAPB" TO OR-SUPPLIER
           MOVE "ZZTEST" TO OR-BUYER
           MOVE 20261006 TO OR-ORDER-DATE
           MOVE "GBP" TO OR-CURRENCY
           MOVE "N30" TO OR-TERMS-CODE
           PERFORM 0980-CALL-ORD THRU 0980-EXIT
           INITIALIZE WS-OR
           MOVE "ADDLINE" TO OC-FUNCTION
           MOVE "ZZ-TEST-GAP-PO3" TO OR-ORDER-NUMBER
           MOVE "STOCK" TO OR-LINE-TYPE
           MOVE "BAR-AL6082-25MM" TO OR-ITEM-CODE
           MOVE "Aluminium bar 6082 25mm" TO OR-DESCRIPTION
           MOVE 10 TO OR-ORDERED-QTY
           MOVE "EA" TO OR-UOM-CODE
           MOVE 5.00 TO OR-UNIT-PRICE
           PERFORM 0980-CALL-ORD THRU 0980-EXIT
           INITIALIZE WS-OR
           MOVE "ISSUE" TO OC-FUNCTION
           MOVE "ZZ-TEST-GAP-PO3" TO OR-ORDER-NUMBER
           PERFORM 0980-CALL-ORD THRU 0980-EXIT
           MOVE "PO-026 a blocked supplier cannot be issued an order"
             TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE OC-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-026 due diligence is the stated reason"
             TO TK-NAME
           MOVE "ORD0013" TO TK-EXPECTED-X
           MOVE OC-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-026 the blocked order is held" TO TK-NAME
           MOVE "HELD" TO TK-EXPECTED-X
           MOVE OR-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Status + '|' + CAST((SELECT COUNT(*) FROM"
                  " po.Commitment c WHERE c.CompanyCode = 'STC' AND"
                  " c.OrderNumber = 'ZZ-TEST-GAP-PO3') AS "
                  "varchar(8)) + '|' + CASE WHEN IssuedAt IS NULL "
                  "THEN 'NOISSUE' ELSE 'ISSUED' END FROM "
                  "po.PurchaseOrder WHERE CompanyCode = 'STC' AND "
                  "OrderNumber = 'ZZ-TEST-GAP-PO3'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0950-READ-ANS THRU 0950-EXIT
           MOVE "PO-026 a held order stays a draft, uncommitted"
             TO TK-NAME
           MOVE "DRAFT|0|NOISSUE" TO TK-EXPECTED-X
           MOVE WS-ANS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       4300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9900-DISCONNECT SECTION.
       9900-START.
           MOVE "DISC" TO SQLIO-FUNC
           MOVE SPACES TO SQLIO-STMT
           CALL "SQLIO" USING SQLIO-CA WS-ANS-ROW.
       9900-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       COPY TESTKITP.
       END PROGRAM TPOGAP.
