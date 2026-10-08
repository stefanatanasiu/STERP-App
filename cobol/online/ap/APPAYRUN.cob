      *>================================================================
      *> PROGRAM-ID : APPAYRUN
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : PAYMENT INSTRUCTIONS AND THEIR OUTCOMES.
      *>              APPROVED, SUBMITTED, ACCEPTED, SETTLED,
      *>              REJECTED, CANCELLED, UNCERTAIN AND RETURNED ARE
      *>              ALL DIFFERENT THINGS (AP-013).  PRODUCING OR
      *>              SUBMITTING A FILE IS NOT PAYMENT: AP IS NOT
      *>              LABELLED PAID UNTIL SETTLEMENT IS CONFIRMED.
      *>              AN UNCERTAIN OUTCOME KEEPS ITS PROTECTIVE
      *>              RESERVATION AND AN INVESTIGATION OWNER; IT DOES
      *>              NOT INVITE AN AUTOMATIC SECOND PAYMENT.
      *>              A REJECTION OR CANCELLATION RELEASES THE
      *>              RESERVATION ONLY WHERE NO SETTLEMENT OCCURRED
      *>              (AP-016).
      *> CALLED BY  : .NET PAYMENT RUN FORM, APBACS, APREMIT
      *> INTERFACE  : CALL "APPAYRUN" USING WS-APPAYRUN-CA
      *>              COPYBOOK APPAYRUNCA
      *> CALLS      : SQLIO, APSVC, APPOST
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1992-11-16 DJM  ORIGINAL BACS SUBMISSION AND RESPONSE LOG
      *> 1997-08-14 RJH  CR0756 RETURNED PAYMENT HANDLING
      *> 2026-10-05 STERP UNCERTAIN OUTCOME AND INVESTIGATION OWNER
      *>================================================================
      *> REQ: AP-013, AP-014, AP-015, AP-016, AP-020, AP-026, D-16
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APPAYRUN.
       AUTHOR. D J MILLER.
       DATE-WRITTEN. 16/11/1992.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY APSVCARE.
       COPY APPOSTWK.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
       01  WS-INS-ROW.
           05  WS-N-ID                 PIC X(24).
           05  WS-N-REF                PIC X(30).
           05  WS-N-SUPPLIER           PIC X(20).
           05  WS-N-METHOD             PIC X(10).
           05  WS-N-BANK               PIC X(20).
           05  WS-N-CURRENCY           PIC X(3).
           05  WS-N-AMOUNT             PIC X(24).
           05  WS-N-AMOUNTGBP          PIC X(24).
           05  WS-N-STATUS             PIC X(12).
           05  WS-N-PROPOSAL           PIC X(24).
           05  WS-N-SETTLED            PIC X(24).
           05  WS-N-VALUEDATE          PIC X(8).
       01  WS-PROP-ROW.
           05  WS-P-REF                PIC X(30).
           05  WS-P-STATUS             PIC X(12).
           05  WS-P-VERSION            PIC X(8).
           05  WS-P-METHOD             PIC X(10).
           05  WS-P-BANK               PIC X(20).
           05  WS-P-CCY                PIC X(3).
           05  WS-P-PAYDATE            PIC X(8).
           05  WS-P-APPR1              PIC X(20).
           05  WS-P-APPR2              PIC X(20).
           05  WS-P-APPROVALID         PIC X(24).
       01  WS-ONE-COL.
           05  WS-C-VALUE              PIC X(40).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-NUM                  PIC S9(15)V9(6) VALUE 0.
           05  WS-COUNT-ED             PIC Z(5)9.
           05  WS-MONEY-ED             PIC -(11)9.99.
       LINKAGE SECTION.
       COPY APPAYRUNCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "CREATE"    PERFORM 2000-CREATE THRU 2000-EXIT
               WHEN "SUBMIT"    PERFORM 3000-SUBMIT THRU 3000-EXIT
               WHEN "OUTCOME"   PERFORM 4000-OUTCOME THRU 4000-EXIT
      *> CR2614 CA-FUNCTION is X(8) - "UNCERTAIN" never matched
               WHEN "UNCERTN"   PERFORM 4000-OUTCOME THRU 4000-EXIT
               WHEN "RETURN"    PERFORM 4000-OUTCOME THRU 4000-EXIT
               WHEN "SETTLE"    PERFORM 5000-SETTLE THRU 5000-EXIT
               WHEN OTHER       PERFORM 9100-BAD-FUNC THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INIT SECTION.
       1000-START.
           MOVE "N" TO WS-FATAL-SW
           MOVE "APPAYRUN" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "AP-013" TO CA-REQ-REF
           MOVE SPACES TO PR-NOTE
           MOVE 0 TO PR-CREATED-CNT
           MOVE 0 TO PR-UPDATED-CNT
           MOVE "N" TO PR-SETTLE-POSTED
           MOVE FUNCTION UPPER-CASE(CA-COMPANY) TO CA-COMPANY
           IF CA-COMPANY = SPACES
               MOVE "APN0001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> CREATE TURNS AN APPROVED PROPOSAL VERSION INTO ONE
      *> INSTRUCTION PER SUPPLIER.  THE BENEFICIARY IS SNAPSHOTTED NOW
      *> (COM-007): A LATER MASTER FILE CHANGE AFFECTS FUTURE
      *> INSTRUCTIONS, NOT THIS ONE.
      *>----------------------------------------------------------------
       2000-CREATE SECTION.
       2000-START.
           INITIALIZE WS-APSVC
           MOVE "PERM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "AP.PAYMENT.RELEASE" TO SVC-TEXT-1
           CALL "APSVC" USING WS-APSVC
           IF SVC-FLAG-1 NOT = "Y"
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APN0002" TO CA-REASON-CODE
               MOVE SVC-MESSAGE TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 2000-EXIT
           END-IF
           PERFORM 7000-READ-PROPOSAL THRU 7000-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           IF WS-P-STATUS NOT = "APPROVED"
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APN0010" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The proposal is "
                      FUNCTION TRIM(WS-P-STATUS)
                      "; only an approved version may be released "
                      "for payment" DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-INSERT-INSTRUCTIONS THRU 2100-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.PaymentProposal SET Status = 'RELEASED',"
                  " ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE ProposalId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE PR-PROPOSAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           MOVE PR-CREATED-CNT TO WS-COUNT-ED
           MOVE 4 TO CA-RETURN-CODE
           MOVE "APN0011" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING FUNCTION TRIM(WS-COUNT-ED)
                  " instruction(s) created and approved. Producing "
                  "an instruction is not payment"
               DELIMITED BY SIZE INTO CA-MESSAGE
           ADD 1 TO CA-MSG-COUNT.
       2000-EXIT.
           EXIT.

       2100-INSERT-INSTRUCTIONS SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ap.PaymentInstruction (CompanyCode, "
                  "InstructionRef, ProposalId, ProposalVersion, "
                  "SupplierCode, PaymentMethod, BankAccountCode, "
                  "CurrencyCode, Amount, AmountGBP, FxRate, "
                  "BeneficiaryName, BeneficiarySortCode, "
                  "BeneficiaryAccountNumber, BeneficiaryIban, "
                  "BeneficiaryBic, SupplierBankId, CopResultSnapshot,"
                  " ValueDate, Status, Authoriser1, Authoriser2, "
                  "ApprovalId, CorrelationId, CreatedBy) "
                  "SELECT p.CompanyCode, p.ProposalRef + '-' + "
                  "RIGHT('000' + CAST(ROW_NUMBER() OVER (ORDER BY "
                  "l.SupplierCode) AS varchar(4)), 3), p.ProposalId, "
                  "p.Version, l.SupplierCode, p.PaymentMethod, "
                  "p.BankAccountCode, p.CurrencyCode, "
                  "SUM(l.NetPayGBP), SUM(l.NetPayGBP), 1, "
                  "MAX(b.BeneficiaryName), MAX(b.SortCode), "
                  "MAX(b.AccountNumber), MAX(b.Iban), MAX(b.Bic), "
                  "MAX(b.SupplierBankId), MAX(b.CopResult), "
                  "p.PayDate, 'APPROVED', p.Approver1, p.Approver2, "
                  "p.ApprovalId, ?, ? FROM ap.PaymentProposalLine l "
                  "JOIN ap.PaymentProposal p ON p.ProposalId = "
                  "l.ProposalId JOIN ap.SupplierBank b ON "
                  "b.SupplierBankId = l.SupplierBankId "
                  "WHERE l.ProposalId = ? AND l.IsExcluded = 0 "
                  "AND NOT EXISTS (SELECT 1 FROM ap.PaymentInstruction"
                  " x WHERE x.ProposalId = l.ProposalId AND "
                  "x.SupplierCode = l.SupplierCode AND x.Status <> "
                  "'CANCELLED') GROUP BY p.CompanyCode, "
                  "p.ProposalRef, p.ProposalId, p.Version, "
                  "l.SupplierCode, p.PaymentMethod, p.BankAccountCode"
                  ", p.CurrencyCode, p.PayDate, p.Approver1, "
                  "p.Approver2, p.ApprovalId HAVING SUM(l.NetPayGBP) "
                  "> 0"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           MOVE PR-PROPOSAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           MOVE SQLIO-ROWCOUNT TO PR-CREATED-CNT.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> SUBMISSION RESERVES THE AMOUNT BUT IS NOT PROOF OF
      *> SETTLEMENT.  THE STATUS MOVES TO SUBMITTED ONLY.
      *>----------------------------------------------------------------
       3000-SUBMIT SECTION.
       3000-START.
           INITIALIZE WS-APSVC
           MOVE "PERM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "AP.PAYMENT.RELEASE" TO SVC-TEXT-1
           CALL "APSVC" USING WS-APSVC
           IF SVC-FLAG-1 NOT = "Y"
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APN0003" TO CA-REASON-CODE
               MOVE SVC-MESSAGE TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.PaymentInstruction SET Status = "
                  "'SUBMITTED', SubmittedAt = SYSDATETIME(), "
                  "SchemeReference = COALESCE(NULLIF(?,''), "
                  "SchemeReference), BacsFileRef = "
                  "COALESCE(NULLIF(?,''), BacsFileRef), ModifiedBy = "
                  "?, ModifiedAt = SYSDATETIME() WHERE CompanyCode = "
                  "? AND Status = 'APPROVED' AND ((? <> 0 AND "
                  "ProposalId = ?) OR (? <> '' AND InstructionRef = "
                  "?))" DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE PR-SCHEME-REF TO SQLIO-PARM-VAL(1)
           MOVE PR-BACS-FILE-REF TO SQLIO-PARM-VAL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(4)
           MOVE PR-PROPOSAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE PR-INSTRUCTION-REF TO SQLIO-PARM-VAL(7)
           MOVE PR-INSTRUCTION-REF TO SQLIO-PARM-VAL(8)
           MOVE 8 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
           MOVE SQLIO-ROWCOUNT TO PR-UPDATED-CNT
           MOVE "SUBMITTED" TO PR-STATUS
           MOVE "Y" TO PR-RESERVED-HELD
           MOVE PR-UPDATED-CNT TO WS-COUNT-ED
           MOVE 4 TO CA-RETURN-CODE
           MOVE "APN0012" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING FUNCTION TRIM(WS-COUNT-ED)
                  " instruction(s) submitted. Submission reserves "
                  "the amount; it is not proof of settlement"
               DELIMITED BY SIZE INTO CA-MESSAGE
           ADD 1 TO CA-MSG-COUNT.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> OUTCOME RECORDS THE BANK'S ANSWER.  REJECTION AND
      *> CANCELLATION RELEASE THE RESERVATION, BUT ONLY WHERE NOTHING
      *> HAS SETTLED.  AN UNCERTAIN OUTCOME KEEPS THE RESERVATION AND
      *> DEMANDS AN INVESTIGATION OWNER.
      *>----------------------------------------------------------------
       4000-OUTCOME SECTION.
       4000-START.
           PERFORM 7100-READ-INSTRUCTION THRU 7100-EXIT
           IF WS-FATAL
               GO TO 4000-EXIT
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "UNCERTAIN" MOVE "UNCERTAIN" TO PR-NEW-STATUS
               WHEN "RETURN"    MOVE "RETURNED" TO PR-NEW-STATUS
           END-EVALUATE
           IF PR-NEW-STATUS = SPACES
               MOVE "APN0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The new payment status must be stated"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           IF PR-NEW-STATUS = "UNCERTAIN"
              AND (PR-INVEST-OWNER = SPACES
                   OR PR-INVEST-OWNER = LOW-VALUES)
               MOVE "APN0013" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "An uncertain payment outcome needs a named "
                      "investigation owner before it can be recorded"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           IF WS-N-STATUS = "SETTLED"
             AND (PR-NEW-STATUS = "CANCELLED"
                  OR PR-NEW-STATUS = "REJECTED")
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APN0014" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A settled payment cannot be cancelled; it is "
                      "corrected by a traceable reversal"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 4000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.PaymentInstruction SET Status = ?, "
                  "OutcomeNote = COALESCE(NULLIF(?,''), OutcomeNote),"
                  " InvestigationOwner = NULLIF(?,''), "
                  "SchemeReference = COALESCE(NULLIF(?,''), "
                  "SchemeReference), ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() WHERE PaymentInstructionId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE PR-NEW-STATUS TO SQLIO-PARM-VAL(1)
           MOVE PR-OUTCOME-NOTE TO SQLIO-PARM-VAL(2)
           MOVE PR-INVEST-OWNER TO SQLIO-PARM-VAL(3)
           MOVE PR-SCHEME-REF TO SQLIO-PARM-VAL(4)
           MOVE CA-USER TO SQLIO-PARM-VAL(5)
           MOVE PR-INSTRUCTION-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-FATAL
               GO TO 4000-EXIT
           END-IF
           MOVE PR-NEW-STATUS TO PR-STATUS
           EVALUATE PR-NEW-STATUS
               WHEN "REJECTED"  PERFORM 4100-RELEASE THRU 4100-EXIT
               WHEN "CANCELLED" PERFORM 4100-RELEASE THRU 4100-EXIT
               WHEN "RETURNED"  PERFORM 4100-RELEASE THRU 4100-EXIT
               WHEN "UNCERTAIN" PERFORM 4200-UNCERTAIN THRU 4200-EXIT
               WHEN OTHER       CONTINUE
           END-EVALUATE
           IF CA-RETURN-CODE = 0
               MOVE SPACES TO CA-MESSAGE
               STRING "Payment outcome recorded as "
                      FUNCTION TRIM(PR-NEW-STATUS)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF
           PERFORM 6000-AUDIT THRU 6000-EXIT.
       4000-EXIT.
           EXIT.

       4100-RELEASE SECTION.
       4100-START.
           MOVE "N" TO PR-RESERVED-HELD
           MOVE "Y" TO PR-ALLOW-REPLACE
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE oi SET ReservedAmountGBP = CASE WHEN "
                  "oi.ReservedAmountGBP - l.ReservedAmountGBP < 0 "
                  "THEN 0 ELSE oi.ReservedAmountGBP - "
                  "l.ReservedAmountGBP END, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() FROM ap.OpenItem oi "
                  "JOIN ap.PaymentProposalLine l ON l.OpenItemId = "
                  "oi.OpenItemId JOIN ap.PaymentInstruction i ON "
                  "i.ProposalId = l.ProposalId AND i.SupplierCode = "
                  "l.SupplierCode WHERE i.PaymentInstructionId = ? "
                  "AND l.ReservedAmountGBP > 0 AND i.SettledAmountGBP"
                  " IS NULL" DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE PR-INSTRUCTION-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           MOVE 4 TO CA-RETURN-CODE
           MOVE "APN0015" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "The reservation was released and the payable is "
                  "eligible again" DELIMITED BY SIZE INTO CA-MESSAGE
           ADD 1 TO CA-MSG-COUNT.
       4100-EXIT.
           EXIT.

       4200-UNCERTAIN SECTION.
       4200-START.
           MOVE "Y" TO PR-RESERVED-HELD
           MOVE "N" TO PR-ALLOW-REPLACE
           INITIALIZE WS-APSVC
           MOVE "EXCEPT" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "HOLD" TO SVC-EXC-SEVERITY
           MOVE "UNCERTAIN PAYMENT" TO SVC-EXC-CATEGORY
           MOVE "PAYMENTINSTR" TO SVC-EXC-DOCTYPE
           MOVE WS-N-REF TO SVC-EXC-DOCREF
           MOVE PR-INVEST-OWNER TO SVC-EXC-OWNER
           MOVE "TREASURY" TO SVC-EXC-ROLE
           COMPUTE SVC-EXC-AMOUNT = FUNCTION NUMVAL(WS-N-AMOUNTGBP)
           MOVE SPACES TO SVC-EXC-MESSAGE
           STRING "The bank outcome of this payment is unknown. The "
                  "amount stays reserved against a duplicate payment "
                  "until the outcome is established"
               DELIMITED BY SIZE INTO SVC-EXC-MESSAGE
           CALL "APSVC" USING WS-APSVC
           MOVE 6 TO CA-RETURN-CODE
           MOVE "APN0016" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Outcome uncertain. The reservation is retained "
                  "and the item is owned for investigation; it must "
                  "not be replaced until the outcome is known"
               DELIMITED BY SIZE INTO CA-MESSAGE
           ADD 1 TO CA-MSG-COUNT.
       4200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> CONFIRMED SETTLEMENT CREATES THE CASH AND AP EFFECT ONCE.  THE
      *> PAYMENT OPEN ITEM, THE ALLOCATION AGAINST THE RESERVED
      *> INVOICES AND THE NOMINAL POSTING ALL HAPPEN HERE, UNDER ONE
      *> CORRELATION ID.
      *>----------------------------------------------------------------
       5000-SETTLE SECTION.
       5000-START.
           INITIALIZE WS-APSVC
           MOVE "PERM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "AP.PAYMENT.POST" TO SVC-TEXT-1
           CALL "APSVC" USING WS-APSVC
           IF SVC-FLAG-1 NOT = "Y"
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APN0005" TO CA-REASON-CODE
               MOVE SVC-MESSAGE TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 5000-EXIT
           END-IF
           PERFORM 7100-READ-INSTRUCTION THRU 7100-EXIT
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
           IF WS-N-STATUS = "SETTLED"
               MOVE 4 TO CA-RETURN-CODE
               MOVE "APN0017" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "That instruction is already settled; it is "
                      "not settled a second time"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 5000-EXIT
           END-IF
           IF WS-N-STATUS = "CANCELLED" OR WS-N-STATUS = "REJECTED"
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APN0018" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A cancelled or rejected instruction cannot "
                      "settle" DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 5000-EXIT
           END-IF
           IF PR-SETTLED-GBP = 0
               COMPUTE PR-SETTLED-GBP =
                   FUNCTION NUMVAL(WS-N-AMOUNTGBP)
           END-IF
           PERFORM 5100-POST-CASH THRU 5100-EXIT
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
           PERFORM 5200-CREATE-OPEN-ITEM THRU 5200-EXIT
           PERFORM 5300-ALLOCATE THRU 5300-EXIT
           PERFORM 5400-MARK-SETTLED THRU 5400-EXIT
           MOVE "SETTLED" TO PR-STATUS
           MOVE "N" TO PR-RESERVED-HELD
           MOVE PR-SETTLED-GBP TO WS-MONEY-ED
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Settlement of GBP "
                  FUNCTION TRIM(WS-MONEY-ED)
                  " confirmed and allocated once"
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 6000-AUDIT THRU 6000-EXIT.
       5000-EXIT.
           EXIT.

       5100-POST-CASH SECTION.
       5100-START.
           INITIALIZE WS-APPOST
           MOVE "POST" TO PJ-FUNC
           MOVE CA-COMPANY TO PJ-COMPANY
           MOVE CA-SITE TO PJ-SITE
           MOVE CA-USER TO PJ-USER
           MOVE CA-BUSINESS-DATE TO PJ-BUSINESS-DATE
           MOVE CA-CORRELATION-ID TO PJ-CORRELATION
           MOVE "AP.PAYMENT" TO PJ-EVENT-CODE
           MOVE "AP" TO PJ-JOURNAL-TYPE
           MOVE WS-N-REF TO PJ-SOURCE-REF
           MOVE SPACES TO PJ-DESCRIPTION
           STRING "Supplier payment " FUNCTION TRIM(WS-N-REF)
                  " to " FUNCTION TRIM(WS-N-SUPPLIER)
               DELIMITED BY SIZE INTO PJ-DESCRIPTION
           IF PR-VALUE-DATE = 0
               COMPUTE WS-NUM = FUNCTION NUMVAL(WS-N-VALUEDATE)
               MOVE WS-NUM TO PR-VALUE-DATE
           END-IF
           MOVE PR-VALUE-DATE TO PJ-POSTING-DATE
           MOVE PR-VALUE-DATE TO PJ-DOCUMENT-DATE
           MOVE "GBP" TO PJ-CURRENCY
           MOVE 1 TO PJ-FX-RATE
           MOVE "JNL" TO PJ-SERIES-CODE
           MOVE "AP" TO PJ-SUBLEDGER-TYPE
           MOVE WS-N-SUPPLIER TO PJ-SUBLEDGER-REF
           MOVE 2 TO PJ-LINE-CNT
           MOVE "2000" TO PJ-L-ACCOUNT(1)
           MOVE PR-SETTLED-GBP TO PJ-L-DR(1)
           MOVE 0 TO PJ-L-CR(1)
           MOVE "Trade creditors settled" TO PJ-L-NARRATIVE(1)
           MOVE "1300" TO PJ-L-ACCOUNT(2)
           MOVE 0 TO PJ-L-DR(2)
           MOVE PR-SETTLED-GBP TO PJ-L-CR(2)
           MOVE WS-N-BANK TO PJ-L-BANK(2)
           MOVE "Bank current account" TO PJ-L-NARRATIVE(2)
           CALL "APPOST" USING WS-APPOST
           IF PJ-RC > 4
               MOVE "Y" TO WS-FATAL-SW
               MOVE PJ-RC TO CA-RETURN-CODE
               MOVE PJ-REASON TO CA-REASON-CODE
               MOVE PJ-MESSAGE TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 5100-EXIT
           END-IF
           MOVE PJ-JOURNAL-NUMBER TO PR-JOURNAL-NUMBER
           MOVE "Y" TO PR-SETTLE-POSTED.
       5100-EXIT.
           EXIT.

       5200-CREATE-OPEN-ITEM SECTION.
       5200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ap.OpenItem (CompanyCode, SupplierCode, "
                  "ItemType, DocumentNumber, DocumentDate, DueDate, "
                  "PostingDate, CurrencyCode, FxRate, OriginalAmount,"
                  " OriginalAmountGBP, ResidualAmount, "
                  "ResidualAmountGBP, SettlementStatus, "
                  "PaymentInstructionId, CreatedBy) "
                  "SELECT ?, ?, 'PAYMENT', ?, CAST(? AS date), "
                  "CAST(? AS date), CAST(? AS date), 'GBP', 1, "
                  "-CAST(? AS decimal(18,2)), "
                  "-CAST(? AS decimal(18,2)), 0, 0, 'SETTLED', ?, ? "
                  "WHERE NOT EXISTS (SELECT 1 FROM ap.OpenItem o "
                  "WHERE o.CompanyCode = ? AND o.ItemType = 'PAYMENT'"
                  " AND o.DocumentNumber = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE WS-N-SUPPLIER TO SQLIO-PARM-VAL(2)
           MOVE WS-N-REF TO SQLIO-PARM-VAL(3)
           MOVE PR-VALUE-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE PR-SETTLED-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE PR-INSTRUCTION-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE CA-USER TO SQLIO-PARM-VAL(10)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(11)
           MOVE WS-N-REF TO SQLIO-PARM-VAL(12)
           MOVE 12 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 12
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       5200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> ALLOCATION AGAINST THE RESERVED INVOICES.  THE RESIDUAL IS
      *> REDUCED AND THE RESERVATION RELEASED IN THE SAME STATEMENT,
      *> SO THE SAME RESIDUAL CANNOT BE CONSUMED TWICE (AP-014).
      *>----------------------------------------------------------------
       5300-ALLOCATE SECTION.
       5300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ap.Allocation (CompanyCode, SupplierCode, "
                  "SourceType, SourceOpenItemId, TargetOpenItemId, "
                  "PaymentInstructionId, EffectiveDate, "
                  "CurrencyCode, AppliedAmount, AppliedAmountGBP, "
                  "DiscountAmountGBP, CreatedBy) "
                  "SELECT i.CompanyCode, i.SupplierCode, 'PAYMENT', "
                  "ISNULL(src.OpenItemId,0), l.OpenItemId, "
                  "i.PaymentInstructionId, i.ValueDate, "
                  "i.CurrencyCode, l.NetPayGBP, l.NetPayGBP, "
                  "l.DiscountGBP, ? FROM ap.PaymentProposalLine l "
                  "JOIN ap.PaymentInstruction i ON i.ProposalId = "
                  "l.ProposalId AND i.SupplierCode = l.SupplierCode "
                  "LEFT JOIN ap.OpenItem src ON src.CompanyCode = "
                  "i.CompanyCode AND src.ItemType = 'PAYMENT' AND "
                  "src.DocumentNumber = i.InstructionRef "
                  "WHERE i.PaymentInstructionId = ? AND "
                  "l.IsExcluded = 0 AND l.NetPayGBP > 0"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE PR-INSTRUCTION-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-FATAL
               GO TO 5300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE oi SET ResidualAmount = oi.ResidualAmount -"
                  " l.NetPayGBP - l.DiscountGBP, ResidualAmountGBP = "
                  "oi.ResidualAmountGBP - l.NetPayGBP - "
                  "l.DiscountGBP, ReservedAmountGBP = CASE WHEN "
                  "oi.ReservedAmountGBP - l.ReservedAmountGBP < 0 "
                  "THEN 0 ELSE oi.ReservedAmountGBP - "
                  "l.ReservedAmountGBP END, SettlementStatus = CASE "
                  "WHEN oi.ResidualAmountGBP - l.NetPayGBP - "
                  "l.DiscountGBP <= 0 THEN 'SETTLED' ELSE 'PARTIAL' "
                  "END, ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "FROM ap.OpenItem oi JOIN ap.PaymentProposalLine l "
                  "ON l.OpenItemId = oi.OpenItemId JOIN "
                  "ap.PaymentInstruction i ON i.ProposalId = "
                  "l.ProposalId AND i.SupplierCode = l.SupplierCode "
                  "WHERE i.PaymentInstructionId = ? AND "
                  "l.IsExcluded = 0 AND l.NetPayGBP > 0"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE PR-INSTRUCTION-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-FATAL
               GO TO 5300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE pi SET SettlementState = CASE WHEN "
                  "oi.SettlementStatus = 'SETTLED' THEN 'SETTLED' "
                  "ELSE 'PARTPAID' END, ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() FROM ap.PurchaseInvoice pi JOIN "
                  "ap.OpenItem oi ON oi.PurchaseInvoiceId = "
                  "pi.PurchaseInvoiceId JOIN ap.Allocation a ON "
                  "a.TargetOpenItemId = oi.OpenItemId WHERE "
                  "a.PaymentInstructionId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE PR-INSTRUCTION-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       5300-EXIT.
           EXIT.

       5400-MARK-SETTLED SECTION.
       5400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.PaymentInstruction SET Status = "
                  "'SETTLED', SettledAt = SYSDATETIME(), "
                  "SettledAmountGBP = ?, OutcomeNote = "
                  "COALESCE(NULLIF(?,''), OutcomeNote), JournalId = "
                  "NULLIF(?,0), InvestigationOwner = NULL, "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE PaymentInstructionId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE PR-SETTLED-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE PR-OUTCOME-NOTE TO SQLIO-PARM-VAL(2)
           MOVE PJ-JOURNAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE CA-USER TO SQLIO-PARM-VAL(4)
           MOVE PR-INSTRUCTION-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       5400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       6000-AUDIT SECTION.
       6000-START.
           INITIALIZE WS-APSVC
           MOVE "AUDIT" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "ap.PaymentInstruction" TO SVC-AUD-ENTITY
           MOVE WS-N-REF TO SVC-AUD-KEY
           MOVE "OUTCOME" TO SVC-AUD-ACTION
           MOVE "Status" TO SVC-AUD-FIELD
           MOVE WS-N-STATUS TO SVC-AUD-OLD
           MOVE PR-STATUS TO SVC-AUD-NEW
           MOVE PR-OUTCOME-NOTE(1:200) TO SVC-AUD-REASON
           MOVE PR-SCHEME-REF(1:40) TO SVC-AUD-AUTH
           MOVE "Y" TO SVC-AUD-SENS
           CALL "APSVC" USING WS-APSVC.
       6000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       7000-READ-PROPOSAL SECTION.
       7000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) p.ProposalRef, p.Status, "
                  "CAST(p.Version AS varchar(8)), p.PaymentMethod, "
                  "p.BankAccountCode, p.CurrencyCode, "
                  "CONVERT(char(8), p.PayDate, 112), "
                  "ISNULL(p.Approver1,''), ISNULL(p.Approver2,''), "
                  "CAST(ISNULL(p.ApprovalId,0) AS varchar(24)) "
                  "FROM ap.PaymentProposal p WHERE p.CompanyCode = ? "
                  "AND ((? <> 0 AND p.ProposalId = ?) OR (? <> '' "
                  "AND p.ProposalRef = ?)) ORDER BY p.Version DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE PR-PROPOSAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE PR-PROPOSAL-REF TO SQLIO-PARM-VAL(4)
           MOVE PR-PROPOSAL-REF TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 10 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-COL-WIDTH(3)
           MOVE 10 TO SQLIO-COL-WIDTH(4)
           MOVE 20 TO SQLIO-COL-WIDTH(5)
           MOVE 3 TO SQLIO-COL-WIDTH(6)
           MOVE 8 TO SQLIO-COL-WIDTH(7)
           MOVE 20 TO SQLIO-COL-WIDTH(8)
           MOVE 20 TO SQLIO-COL-WIDTH(9)
           MOVE 24 TO SQLIO-COL-WIDTH(10)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROP-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 7000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE
               MOVE "APN0006" TO CA-REASON-CODE
               MOVE "That payment proposal was not found"
                   TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 7000-EXIT
           END-IF
           MOVE WS-P-REF TO PR-PROPOSAL-REF
           MOVE WS-P-METHOD TO PR-METHOD
           MOVE WS-P-BANK TO PR-BANK-ACCOUNT
           MOVE WS-P-CCY TO PR-CURRENCY
           MOVE WS-P-APPR1 TO PR-AUTHORISER-1
           MOVE WS-P-APPR2 TO PR-AUTHORISER-2
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-P-VERSION)
           MOVE WS-NUM TO PR-VERSION
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-P-PAYDATE)
           MOVE WS-NUM TO PR-VALUE-DATE.
       7000-EXIT.
           EXIT.

       7100-READ-INSTRUCTION SECTION.
       7100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CAST(PaymentInstructionId AS "
                  "varchar(24)), InstructionRef, SupplierCode, "
                  "PaymentMethod, BankAccountCode, CurrencyCode, "
                  "CAST(Amount AS varchar(24)), CAST(AmountGBP AS "
                  "varchar(24)), Status, CAST(ISNULL(ProposalId,0) "
                  "AS varchar(24)), CAST(ISNULL(SettledAmountGBP,0) "
                  "AS varchar(24)), CONVERT(char(8), ValueDate, 112) "
                  "FROM ap.PaymentInstruction WHERE CompanyCode = ? "
                  "AND ((? <> 0 AND PaymentInstructionId = ?) OR "
                  "(? <> '' AND InstructionRef = ?))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE PR-INSTRUCTION-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE PR-INSTRUCTION-REF TO SQLIO-PARM-VAL(4)
           MOVE PR-INSTRUCTION-REF TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 12 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 20 TO SQLIO-COL-WIDTH(3)
           MOVE 10 TO SQLIO-COL-WIDTH(4)
           MOVE 20 TO SQLIO-COL-WIDTH(5)
           MOVE 3 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 24 TO SQLIO-COL-WIDTH(8)
           MOVE 12 TO SQLIO-COL-WIDTH(9)
           MOVE 24 TO SQLIO-COL-WIDTH(10)
           MOVE 24 TO SQLIO-COL-WIDTH(11)
           MOVE 8 TO SQLIO-COL-WIDTH(12)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-INS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 7100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE
               MOVE "APN0007" TO CA-REASON-CODE
               MOVE "That payment instruction was not found"
                   TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 7100-EXIT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-N-ID)
           MOVE WS-NUM TO PR-INSTRUCTION-ID
           MOVE WS-N-REF TO PR-INSTRUCTION-REF
           MOVE WS-N-SUPPLIER TO PR-SUPPLIER
           MOVE WS-N-METHOD TO PR-METHOD
           MOVE WS-N-BANK TO PR-BANK-ACCOUNT
           MOVE WS-N-CURRENCY TO PR-CURRENCY
           COMPUTE PR-AMOUNT = FUNCTION NUMVAL(WS-N-AMOUNT)
           COMPUTE PR-AMOUNT-GBP = FUNCTION NUMVAL(WS-N-AMOUNTGBP)
           MOVE WS-N-STATUS TO PR-STATUS
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-N-PROPOSAL)
           MOVE WS-NUM TO PR-PROPOSAL-ID.
       7100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8500-EXEC-UPDATE SECTION.
       8500-START.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 8500-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       8500-EXIT.
           EXIT.

       8900-SVC-HEADER SECTION.
       8900-START.
           MOVE CA-COMPANY TO SVC-COMPANY
           MOVE CA-SITE TO SVC-SITE
           MOVE CA-USER TO SVC-USER
           MOVE CA-BUSINESS-DATE TO SVC-DATE
           MOVE CA-CORRELATION-ID TO SVC-CORRELATION.
       8900-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-FATAL-SW
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNC SECTION.
       9100-START.
           MOVE "APN0099" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "APPAYRUN does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE "Y" TO WS-FATAL-SW
           MOVE 12 TO CA-RETURN-CODE
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF CA-MESSAGE = SPACES
               MOVE "SQL failure in APPAYRUN" TO CA-MESSAGE
           END-IF
           ADD 1 TO CA-MSG-COUNT
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       9300-EXIT.
           EXIT.
