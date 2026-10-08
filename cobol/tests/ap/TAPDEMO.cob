      *>================================================================
      *> PROGRAM-ID : TAPDEMO
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (AP) UNIT TESTS
      *> PURPOSE    : THE DEMONSTRATION PAYMENT RUN THAT APDEMO BUILDS.
      *>              THE SUITE DRIVES THE SAME COMMAREA PROGRAMS IN
      *>              THE SAME ORDER - APPAYPRP RESERVE, APPAYAPR
      *>              REQUEST, TWO INDEPENDENT DECIDES, APPAYRUN
      *>              CREATE - AND PROVES WHAT APBACS NEEDS BEFORE IT
      *>              WILL CUT A STANDARD 18 FILE AT CONDITION CODE 0:
      *>              A RELEASED RUN, TWO AUTHORISERS WHO ARE NEITHER
      *>              EACH OTHER NOR THE PREPARER, A CONTENT HASH
      *>              THAT STILL MATCHES, INSTRUCTIONS VISIBLE IN
      *>              ap.vw_BacsInstruction AND NOT ONE OF THEM
      *>              ALREADY CARRYING A BACS FILE REFERENCE.
      *>              THE SUITE PLANTS ITS OWN PAYABLE SO IT DOES NOT
      *>              DEPEND ON WHAT THE DEMONSTRATION DATA HAS
      *>              ALREADY RESERVED, AND REMOVES EVERYTHING IT
      *>              CREATED BEFORE IT ENDS.  IT NEEDS THE DATABASE.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: AP-011, AP-012, AP-013, AP-015, AP-020, D-07
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAPDEMO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY SQLIOCA.
       COPY APPAYPRPCA REPLACING ==DFHCOMMAREA== BY ==WS-PRP-CA==.
       COPY APPAYAPRCA REPLACING ==DFHCOMMAREA== BY ==WS-APR-CA==.
       COPY APPAYRUNCA REPLACING ==DFHCOMMAREA== BY ==WS-RUN-CA==.
       01  WS-ROW.
           05  WS-COL OCCURS 6 TIMES   PIC X(64).
       01  WS-KEYS.
           05  K-CO                    PIC X(10) VALUE "STC".
           05  K-DOC                   PIC X(30) VALUE "TAPDEMO-001".
           05  K-PREPARER              PIC X(20) VALUE "APCLERK".
           05  K-AUTH-1                PIC X(20) VALUE "TREASURY".
           05  K-AUTH-2                PIC X(20) VALUE "TREASURY2".
           05  K-RELEASER              PIC X(20) VALUE "TREASURY".
       01  WS-WORK.
           05  W-I                     PIC S9(4) VALUE 0.
           05  W-TODAY                 PIC 9(8) VALUE 0.
           05  W-PAY-DATE              PIC 9(8) VALUE 0.
           05  W-CUTOFF                PIC 9(8) VALUE 0.
           05  W-NOW                   PIC X(21) VALUE SPACES.
           05  W-REF                   PIC X(30) VALUE SPACES.
           05  W-ID                    PIC S9(9) VALUE 0.
           05  W-APPROVAL-ID           PIC S9(9) VALUE 0.
           05  W-COUNT                 PIC S9(9) VALUE 0.
           05  W-FLAG                  PIC X VALUE "N".
           05  W-NUM-ED                PIC -(9)9.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TAPDEMO demonstration payment run" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-SETUP
           PERFORM 1000-PROPOSE
           PERFORM 1100-REQUEST
           PERFORM 1200-PREPARER-CANNOT-APPROVE
           PERFORM 1300-FIRST-AUTHORISER
           PERFORM 1400-SECOND-AUTHORISER
           PERFORM 1500-RELEASE
           PERFORM 1600-READY-FOR-BACS
           PERFORM 1700-VERIFY-AFTER-RELEASE
           PERFORM 9000-CLEAN-UP
           PERFORM TK-END
           STOP RUN.

      *>   a payable of its own, cloned from a payable that the
      *>   purchase ledger already knows is payable
       0100-SETUP.
           MOVE FUNCTION CURRENT-DATE TO W-NOW
           COMPUTE W-TODAY = FUNCTION NUMVAL(W-NOW(1:8))
           COMPUTE W-PAY-DATE = FUNCTION DATE-OF-INTEGER(
               FUNCTION INTEGER-OF-DATE(W-TODAY) + 7)
           MOVE 20271231 TO W-CUTOFF
           PERFORM 9100-REMOVE-TEST-ITEM
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ap.OpenItem (CompanyCode, SupplierCode, "
                  "ItemType, DocumentNumber, SupplierDocRef, "
                  "DocumentDate, DueDate, PostingDate, FiscalYear, "
                  "PeriodNo, CurrencyCode, FxRate, OriginalAmount, "
                  "OriginalAmountGBP, ResidualAmount, "
                  "ResidualAmountGBP, SettlementStatus, CreatedBy) "
                  "SELECT TOP (1) o.CompanyCode, o.SupplierCode, "
                  "'INVOICE', ?, ?, o.DocumentDate, o.DueDate, "
                  "o.PostingDate, o.FiscalYear, o.PeriodNo, 'GBP', 1,"
                  " 1234.56, 1234.56, 1234.56, 1234.56, 'OPEN', "
                  "'TAPDEMO' FROM ap.OpenItem o JOIN ap.Supplier s "
                  "ON s.CompanyCode = o.CompanyCode AND "
                  "s.SupplierCode = o.SupplierCode WHERE "
                  "o.CompanyCode = ? AND o.ItemType = 'INVOICE' AND "
                  "o.CurrencyCode = 'GBP' AND o.IsBlocked = 0 AND "
                  "o.IsDisputed = 0 AND s.PaymentHold = 0 AND "
                  "s.Status <> 'PENDING' AND EXISTS (SELECT 1 FROM "
                  "ap.SupplierBank b WHERE b.CompanyCode = "
                  "o.CompanyCode AND b.SupplierCode = o.SupplierCode "
                  "AND b.Status = 'APPROVED') ORDER BY o.OpenItemId"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE K-DOC TO SQLIO-PARM-VAL(1)
           MOVE K-DOC TO SQLIO-PARM-VAL(2)
           MOVE K-CO TO SQLIO-PARM-VAL(3)
           PERFORM 9200-NO-NULLS
           PERFORM 9300-EXEC
           MOVE "a payable is waiting to be paid" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE SQLIO-ROWCOUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: AP-011, AP-020 - selection and reservation together
       1000-PROPOSE.
           INITIALIZE WS-PRP-CA
           MOVE "RESERVE" TO CA-FUNCTION OF WS-PRP-CA
           MOVE K-CO TO CA-COMPANY OF WS-PRP-CA
           MOVE K-PREPARER TO CA-USER OF WS-PRP-CA
           MOVE W-TODAY TO CA-BUSINESS-DATE OF WS-PRP-CA
           MOVE 1 TO PP-VERSION
           MOVE W-CUTOFF TO PP-CUTOFF-DATE
           MOVE W-PAY-DATE TO PP-PAY-DATE
           MOVE "BACS" TO PP-METHOD
           MOVE "GBP1" TO PP-BANK-ACCOUNT
           MOVE "GBP" TO PP-CURRENCY
           MOVE 250000 TO PP-CHAPS-THRESHOLD
           MOVE 1000000 TO PP-SINGLE-LIMIT
           CALL "APPAYPRP" USING WS-PRP-CA
           MOVE PP-PROPOSAL-REF TO W-REF
           MOVE PP-PROPOSAL-ID TO W-ID
           MOVE "the proposal was built and reserved" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           IF CA-RETURN-CODE OF WS-PRP-CA <= 4 AND W-ID > 0
               MOVE "Y" TO TK-ACTUAL-X
           ELSE
               MOVE "N" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR
           MOVE "at least one payable was selected" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           IF PP-SELECTED-CNT > 0
               MOVE "Y" TO TK-ACTUAL-X
           ELSE
               MOVE "N" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR.

      *>   REQ: AP-012 - APHASH builds the content hash, not a seed
       1100-REQUEST.
           INITIALIZE WS-APR-CA
           MOVE "REQUEST" TO CA-FUNCTION OF WS-APR-CA
           MOVE K-CO TO CA-COMPANY OF WS-APR-CA
           MOVE K-PREPARER TO CA-USER OF WS-APR-CA
           MOVE W-TODAY TO CA-BUSINESS-DATE OF WS-APR-CA
           MOVE W-REF TO PA-PROPOSAL-REF
           MOVE W-ID TO PA-PROPOSAL-ID
           CALL "APPAYAPR" USING WS-APR-CA
           MOVE PA-APPROVAL-ID TO W-APPROVAL-ID
           MOVE "approval of the exact content was requested"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-APR-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the content hash was computed" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           IF PA-CONTENT-HASH NOT = SPACES
               MOVE "Y" TO TK-ACTUAL-X
           ELSE
               MOVE "N" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR.

      *>   REQ: D-07, COM-003 - the preparer may never authorise
       1200-PREPARER-CANNOT-APPROVE.
           INITIALIZE WS-APR-CA
           MOVE "DECIDE" TO CA-FUNCTION OF WS-APR-CA
           MOVE K-CO TO CA-COMPANY OF WS-APR-CA
           MOVE K-PREPARER TO CA-USER OF WS-APR-CA
           MOVE W-TODAY TO CA-BUSINESS-DATE OF WS-APR-CA
           MOVE W-REF TO PA-PROPOSAL-REF
           MOVE W-ID TO PA-PROPOSAL-ID
           MOVE W-APPROVAL-ID TO PA-APPROVAL-ID
           MOVE "APPROVED" TO PA-DECISION
           CALL "APPAYAPR" USING WS-APR-CA
           MOVE "the preparer cannot approve the run" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           IF CA-RETURN-CODE OF WS-APR-CA >= 6
               MOVE "Y" TO TK-ACTUAL-X
           ELSE
               MOVE "N" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR.

      *>   REQ: D-07 - one signature is not enough
       1300-FIRST-AUTHORISER.
           PERFORM 1900-DECIDE-AS
           MOVE "one signature leaves the run unapproved" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-APR-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       1400-SECOND-AUTHORISER.
           MOVE K-AUTH-2 TO K-AUTH-1
           PERFORM 1900-DECIDE-AS
           MOVE "the second authoriser approves the run" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-APR-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the proposal is APPROVED" TO TK-NAME
           MOVE "APPROVED" TO TK-EXPECTED-X
           MOVE PA-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       1900-DECIDE-AS.
           INITIALIZE WS-APR-CA
           MOVE "DECIDE" TO CA-FUNCTION OF WS-APR-CA
           MOVE K-CO TO CA-COMPANY OF WS-APR-CA
           MOVE K-AUTH-1 TO CA-USER OF WS-APR-CA
           MOVE W-TODAY TO CA-BUSINESS-DATE OF WS-APR-CA
           MOVE W-REF TO PA-PROPOSAL-REF
           MOVE W-ID TO PA-PROPOSAL-ID
           MOVE W-APPROVAL-ID TO PA-APPROVAL-ID
           MOVE "APPROVED" TO PA-DECISION
           MOVE "TAPDEMO" TO PA-COMMENT
           CALL "APPAYAPR" USING WS-APR-CA.

      *>   REQ: AP-013 - releasing turns the content into instructions
       1500-RELEASE.
           INITIALIZE WS-RUN-CA
           MOVE "CREATE" TO CA-FUNCTION OF WS-RUN-CA
           MOVE K-CO TO CA-COMPANY OF WS-RUN-CA
           MOVE K-RELEASER TO CA-USER OF WS-RUN-CA
           MOVE W-TODAY TO CA-BUSINESS-DATE OF WS-RUN-CA
           MOVE W-REF TO PR-PROPOSAL-REF
           MOVE W-ID TO PR-PROPOSAL-ID
           MOVE "BACS" TO PR-METHOD
           CALL "APPAYRUN" USING WS-RUN-CA
           MOVE "payment instructions were created" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           IF PR-CREATED-CNT > 0
               MOVE "Y" TO TK-ACTUAL-X
           ELSE
               MOVE "N" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR
           MOVE "an instruction is not a payment" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-RUN-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: AP-015 - everything APBACS needs for a clean first cut
       1600-READY-FOR-BACS.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(12)) FROM "
                  "ap.vw_BacsInstruction WHERE CompanyCode = ? AND "
                  "ProposalRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 9400-RUN-COUNT
           MOVE "the run is visible to the Bacs cut" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           IF W-COUNT > 0
               MOVE "Y" TO TK-ACTUAL-X
           ELSE
               MOVE "N" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(12)) FROM "
                  "ap.PaymentInstruction i JOIN ap.PaymentProposal p "
                  "ON p.ProposalId = i.ProposalId WHERE "
                  "p.CompanyCode = ? AND p.ProposalRef = ? AND "
                  "(i.BacsFileRef IS NOT NULL OR i.Status <> "
                  "'APPROVED')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 9400-RUN-COUNT
           MOVE "nothing has been sent to the bureau yet" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE W-COUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(12)) FROM "
                  "ap.PaymentProposal p WHERE p.CompanyCode = ? AND "
                  "p.ProposalRef = ? AND p.Status = 'RELEASED' AND "
                  "p.Approver1 IS NOT NULL AND p.Approver2 IS NOT "
                  "NULL AND p.Approver1 <> p.Approver2 AND "
                  "p.Approver1 <> p.PreparedBy AND p.Approver2 <> "
                  "p.PreparedBy"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 9400-RUN-COUNT
           MOVE "three different people built and signed it"
               TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE W-COUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: AP-012, AP-013 - a released run has no live approval
      *>   to VERIFY, which is the APA0020 answer APBACS reads before
      *>   deciding whether its file is an original or a reproduction
       1700-VERIFY-AFTER-RELEASE.
           INITIALIZE WS-APR-CA
           MOVE "VERIFY" TO CA-FUNCTION OF WS-APR-CA
           MOVE K-CO TO CA-COMPANY OF WS-APR-CA
           MOVE K-RELEASER TO CA-USER OF WS-APR-CA
           MOVE W-TODAY TO CA-BUSINESS-DATE OF WS-APR-CA
           MOVE W-REF TO PA-PROPOSAL-REF
           MOVE W-ID TO PA-PROPOSAL-ID
           CALL "APPAYAPR" USING WS-APR-CA
           MOVE "a released run answers APA0020" TO TK-NAME
           MOVE "APA0020" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-APR-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the approved content still matches" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           IF PA-CONTENT-HASH = PA-STORED-HASH
              AND PA-CONTENT-HASH NOT = SPACES
               MOVE "Y" TO TK-ACTUAL-X
           ELSE
               MOVE "N" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *>   Everything the suite created goes away again, reservations
      *>   first, so a second run starts from the same place.
       9000-CLEAN-UP.
           IF W-ID = 0
               GO TO 9000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE oi SET ReservedAmountGBP = CASE WHEN "
                  "oi.ReservedAmountGBP - l.ReservedAmountGBP < 0 "
                  "THEN 0 ELSE oi.ReservedAmountGBP - "
                  "l.ReservedAmountGBP END FROM ap.OpenItem oi JOIN "
                  "ap.PaymentProposalLine l ON l.OpenItemId = "
                  "oi.OpenItemId WHERE l.ProposalId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 9500-EXEC-BY-ID
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE ap.PaymentInstruction WHERE ProposalId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 9500-EXEC-BY-ID
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE ap.PaymentProposalLine WHERE ProposalId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 9500-EXEC-BY-ID
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE core.Approval WHERE DocumentRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE W-REF TO SQLIO-PARM-VAL(1)
           PERFORM 9200-NO-NULLS
           PERFORM 9300-EXEC
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE ap.PaymentProposal WHERE ProposalId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 9500-EXEC-BY-ID
           PERFORM 9100-REMOVE-TEST-ITEM
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(12)) FROM "
                  "ap.PaymentProposal p WHERE p.CompanyCode = ? AND "
                  "p.ProposalRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 9400-RUN-COUNT
           MOVE "the suite leaves nothing behind" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE W-COUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       9000-EXIT.
           EXIT.

       9100-REMOVE-TEST-ITEM.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE ap.OpenItem WHERE CompanyCode = ? AND "
                  "DocumentNumber = ? AND CreatedBy = 'TAPDEMO'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE K-CO TO SQLIO-PARM-VAL(1)
           MOVE K-DOC TO SQLIO-PARM-VAL(2)
           PERFORM 9200-NO-NULLS
           PERFORM 9300-EXEC.

       9200-NO-NULLS.
           PERFORM VARYING W-I FROM 1 BY 1
                   UNTIL W-I > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(W-I)
           END-PERFORM.

       9300-EXEC.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

       9400-RUN-COUNT.
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE K-CO TO SQLIO-PARM-VAL(1)
           MOVE W-REF TO SQLIO-PARM-VAL(2)
           PERFORM 9200-NO-NULLS
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE 0 TO W-COUNT
           IF NOT SQLIO-ERROR AND NOT SQLIO-NOTFOUND
               COMPUTE W-COUNT = FUNCTION NUMVAL(WS-COL(1))
           END-IF.

       9500-EXEC-BY-ID.
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE W-ID TO W-NUM-ED
           MOVE FUNCTION TRIM(W-NUM-ED) TO SQLIO-PARM-VAL(1)
           PERFORM 9200-NO-NULLS
           PERFORM 9300-EXEC.
       COPY TESTKITP.
