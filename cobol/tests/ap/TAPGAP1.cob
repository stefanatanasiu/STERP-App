      *>================================================================
      *> PROGRAM-ID : TAPGAP1
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (AP) UNIT TESTS
      *> PURPOSE    : THE SUPPLIER ACCOUNT, THE PAYMENT BLOCK AND THE
      *>              SUPPLIER STATEMENT - THE THREE PLACES WHERE AN
      *>              AP CONTROL IS EASILY CONFUSED WITH AN ACCOUNTING
      *>              STATE.
      *>                APSUPVAL - SUSPENDING NEW PURCHASING NEITHER
      *>                  ERASES THE EXISTING LIABILITY NOR FORBIDS
      *>                  PAYING IT; THE TWO HOLDS ARE SEPARATE
      *>                  POLICIES (AP-001, AC-028)
      *>                APBLOCK  - A POSTED LIABILITY CAN STAY BLOCKED
      *>                  FOR PAYMENT AND A MATCH DISPUTE IS NOT A
      *>                  CLEAN MATCH (AP-008); THE CONTRACTUAL DUE
      *>                  DATE COMES FROM THE RECORDED TERMS AND A
      *>                  LATE INTERNAL RECEIPT DOES NOT RESTART IT
      *>                  (AP-009, AC-031)
      *>                APSTMREC - AN UNMATCHED STATEMENT LINE IS A
      *>                  REVIEW ITEM, NOT AUTHORITY TO POST OR PAY
      *>                  (AP-019, AC-037)
      *>              EVERY FIXTURE CARRIES THE PREFIX ZZAPG AND IS
      *>              REMOVED BEFORE AND AFTER THE RUN, SO THE SUITE
      *>              IS REPEATABLE.  WITH NO DATABASE IT SKIPS
      *>              RATHER THAN FAILS.
      *>----------------------------------------------------------------
      *> 2026-10-07 STERP ORIGINAL
      *>================================================================
      *> REQ: AP-001, AP-008, AP-009, AP-019   AC: AC-028, AC-031
      *> AC: AC-037
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAPGAP1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY APSUPVALCA REPLACING ==DFHCOMMAREA== BY ==WS-SUP-CA==.
       COPY APBLOCKCA REPLACING ==DFHCOMMAREA== BY ==WS-BLK-CA==.
       COPY APSTMRECCA REPLACING ==DFHCOMMAREA== BY ==WS-STM-CA==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "Y".
       01  WS-PROBE.
           05  WS-PROBE-1              PIC X(20).
           05  WS-PROBE-2              PIC X(20).
       01  WS-ROW2.
           05  WS-R2-ONE               PIC X(40).
           05  WS-R2-TWO               PIC X(40).
       01  WS-WORK.
           05  WS-NUM                  PIC S9(15)V9(6) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TAPGAP1 holds, blocks, statements" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-EVERYTHING
           ELSE
               PERFORM 0200-CLEAN-UP
               PERFORM 0300-MAKE-SUPPLIERS
               PERFORM 0310-MAKE-INVOICE
               PERFORM 0320-MAKE-OPEN-ITEMS
               PERFORM 1000-SUPPLIER-IS-PAYABLE
               PERFORM 1100-PURCHASE-HOLD-KEEPS-DEBT
               PERFORM 1200-HOLD-NEEDS-A-REASON
               PERFORM 1300-PAYMENT-HOLD-IS-SEPARATE
               PERFORM 1400-DUE-DATE-FROM-TERMS
               PERFORM 1450-DUE-DATE-OVERRIDE
               PERFORM 1500-BLOCK-NEEDS-A-REASON
               PERFORM 1600-POSTED-BUT-BLOCKED
               PERFORM 1700-DISPUTE-IS-NOT-A-MATCH
               PERFORM 1800-RELEASE-NEEDS-OUTCOME
               PERFORM 2000-STATEMENT-LOADED
               PERFORM 2100-STATEMENT-RECONCILED
               PERFORM 2200-NOTHING-WAS-POSTED
               PERFORM 0200-CLEAN-UP
           END-IF
           PERFORM TK-END
           STOP RUN.

       0100-PROBE-DATABASE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) SupplierCode, '' FROM ap.Supplier "
                  "WHERE CompanyCode = 'STC' AND SupplierCode = "
                  "'S0001'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "N" TO WS-DB-UP
           END-IF.

       0900-SKIP-EVERYTHING.
           MOVE "purchase ledger holds, blocks and statements"
               TO TK-NAME
           MOVE "the purchase ledger tables are not deployed"
               TO TK-DETAIL
           PERFORM TK-SKIP.

      *>   remove anything an earlier run of this suite left behind
       0200-CLEAN-UP.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; DELETE ap.SupplierStatementLine "
                  "WHERE StatementId IN (SELECT StatementId FROM "
                  "ap.SupplierStatement WHERE CompanyCode = 'STC' "
                  "AND StatementRef LIKE 'ZZAPGAP%'); "
                  "DELETE ap.SupplierStatement WHERE CompanyCode = "
                  "'STC' AND StatementRef LIKE 'ZZAPGAP%'; "
                  "DELETE core.ExceptionItem WHERE CompanyCode = "
                  "'STC' AND DocumentRef LIKE 'ZZAPGAP%'; "
                  "DELETE ap.Allocation WHERE CompanyCode = 'STC' "
                  "AND SupplierCode LIKE 'ZZAPG%'; "
                  "DELETE ap.OpenItem WHERE CompanyCode = 'STC' AND "
                  "SupplierCode LIKE 'ZZAPG%'; "
                  "DELETE ap.PurchaseInvoice WHERE CompanyCode = "
                  "'STC' AND SupplierCode LIKE 'ZZAPG%'; "
                  "DELETE core.AuditLog WHERE CompanyCode = 'STC' "
                  "AND EntityKey LIKE 'ZZAPG%'; "
                  "DELETE ap.Supplier WHERE CompanyCode = 'STC' AND "
                  "SupplierCode LIKE 'ZZAPG%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8000-EXEC-SQL.

       0300-MAKE-SUPPLIERS.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT ap.Supplier (CompanyCode, "
                  "SupplierCode, LegalName, TermsCode, CurrencyCode, "
                  "PaymentMethod, Status, BusinessSize, CreatedBy) "
                  "VALUES ('STC','ZZAPG1','Gap Test Fasteners Ltd',"
                  "'D2N30','GBP','BACS','ACTIVE','SMALL','TEST'), "
                  "('STC','ZZAPG2','Gap Test Castings Ltd','N30',"
                  "'GBP','BACS','ACTIVE','MEDIUM','TEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8000-EXEC-SQL.

      *>   a genuine, already posted invoice: the supplier's document
      *>   is dated 10 September but we only opened the post on
      *>   5 October
       0310-MAKE-INVOICE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT ap.PurchaseInvoice "
                  "(CompanyCode, RegisterNumber, SupplierCode, "
                  "SupplierDocRef, SupplierDocRefNormalised, "
                  "DocumentType, DocumentDate, DateReceived, "
                  "TaxPointDate, DueDate, CurrencyCode, "
                  "NetAmountGBP, VatAmountGBP, GrossAmountGBP, "
                  "TermsCode, RegistrationState, MatchState, "
                  "AccountingState, PaymentEligibility, "
                  "SettlementState, OwnerUser, CreatedBy) VALUES "
                  "('STC','ZZAPGREG1','ZZAPG1','ZZAPG/DOC/1',"
                  "'ZZAPG/DOC/1','INVOICE','2026-09-10',"
                  "'2026-10-05','2026-09-10','2026-10-10','GBP',"
                  "1000.00,200.00,1200.00,'D2N30','READY',"
                  "'UNMATCHED','POSTED','APPROVED','UNPAID',"
                  "'APCLERK','TEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8000-EXEC-SQL.

      *>   ZZAPG1 owes 1,200.00.  ZZAPG2 carries the statement case of
      *>   AC-037: an open item, a blocked one and one we have paid.
       0320-MAKE-OPEN-ITEMS.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT ap.OpenItem (CompanyCode, "
                  "SupplierCode, ItemType, DocumentNumber, "
                  "SupplierDocRef, DocumentDate, DueDate, "
                  "PostingDate, DateReceived, CurrencyCode, FxRate, "
                  "OriginalAmount, OriginalAmountGBP, "
                  "ResidualAmount, ResidualAmountGBP, TermsCode, "
                  "SettlementStatus, IsBlocked, PurchaseInvoiceId, "
                  "CreatedBy) SELECT 'STC','ZZAPG1','INVOICE',"
                  "'ZZAPG/INV/1','ZZAPG/DOC/1','2026-09-10',"
                  "'2026-10-10','2026-09-10','2026-10-05','GBP',1,"
                  "1200.00,1200.00,1200.00,1200.00,'D2N30','OPEN',0,"
                  "p.PurchaseInvoiceId,'TEST' FROM "
                  "ap.PurchaseInvoice p WHERE p.CompanyCode = 'STC' "
                  "AND p.RegisterNumber = 'ZZAPGREG1'; "
                  "INSERT ap.OpenItem (CompanyCode, SupplierCode, "
                  "ItemType, DocumentNumber, DocumentDate, DueDate, "
                  "PostingDate, CurrencyCode, FxRate, "
                  "OriginalAmount, OriginalAmountGBP, "
                  "ResidualAmount, ResidualAmountGBP, TermsCode, "
                  "SettlementStatus, IsBlocked, CreatedBy) VALUES "
                  "('STC','ZZAPG2','INVOICE','ZZAPG/S/100',"
                  "'2026-09-01','2026-10-01','2026-09-01','GBP',1,"
                  "500.00,500.00,500.00,500.00,'N30','OPEN',0,"
                  "'TEST'), ('STC','ZZAPG2','INVOICE','ZZAPG/S/200',"
                  "'2026-09-02','2026-10-02','2026-09-02','GBP',1,"
                  "300.00,300.00,300.00,300.00,'N30','OPEN',1,"
                  "'TEST'), ('STC','ZZAPG2','INVOICE','ZZAPG/S/300',"
                  "'2026-09-03','2026-10-03','2026-09-03','GBP',1,"
                  "400.00,400.00,0.00,0.00,'N30','SETTLED',0,'TEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8000-EXEC-SQL.

      *>   the common header for every APSUPVAL call
       0400-SUPPLIER-HEADER.
           INITIALIZE WS-SUP-CA
           MOVE "STC" TO CA-COMPANY OF WS-SUP-CA
           MOVE "RDG" TO CA-SITE OF WS-SUP-CA
           MOVE "APMGR" TO CA-USER OF WS-SUP-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-SUP-CA
           MOVE "ZZAPG1" TO SV-SUPPLIER.

       0500-BLOCK-HEADER.
           INITIALIZE WS-BLK-CA
           MOVE "STC" TO CA-COMPANY OF WS-BLK-CA
           MOVE "RDG" TO CA-SITE OF WS-BLK-CA
           MOVE "APMGR" TO CA-USER OF WS-BLK-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-BLK-CA.

      *>   AP-001 - a clean account may be bought from and paid, and
      *>   the ledger balance behind it is reported with it
       1000-SUPPLIER-IS-PAYABLE.
           PERFORM 0400-SUPPLIER-HEADER
           MOVE "ZZAPGAP-VAL1" TO CA-CORRELATION-ID OF WS-SUP-CA
           MOVE "VALIDATE" TO CA-FUNCTION OF WS-SUP-CA
           CALL "APSUPVAL" USING WS-SUP-CA
           MOVE "an active supplier validates cleanly" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-SUP-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-SUP-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "it may be purchased from" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SV-CAN-PURCHASE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "it may be paid" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SV-CAN-PAY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the open balance is 1,200.00" TO TK-NAME
           MOVE 1200.00 TO TK-EXPECTED-N
           MOVE SV-OPEN-BAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "one open item stands behind it" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE SV-OPEN-ITEM-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AP-001 / AC-028 - suspending new purchasing does not erase
      *>   the liability and does not forbid paying it
       1100-PURCHASE-HOLD-KEEPS-DEBT.
           PERFORM 0400-SUPPLIER-HEADER
           MOVE "ZZAPGAP-HOLD1" TO CA-CORRELATION-ID OF WS-SUP-CA
           MOVE "HOLD" TO CA-FUNCTION OF WS-SUP-CA
           MOVE "PURCHASE" TO SV-HOLD-TYPE
           MOVE "Quality investigation - buy nothing further"
               TO SV-REASON
           CALL "APSUPVAL" USING WS-SUP-CA
           MOVE "a purchasing hold is applied without error"
               TO TK-NAME
           MOVE CA-MESSAGE OF WS-SUP-CA TO TK-DETAIL
           IF CA-RETURN-CODE OF WS-SUP-CA < 8
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           MOVE "the hold is recorded against the account"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SV-PURCHASE-HOLD TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0400-SUPPLIER-HEADER
           MOVE "ZZAPGAP-VAL2" TO CA-CORRELATION-ID OF WS-SUP-CA
           MOVE "VALIDATE" TO CA-FUNCTION OF WS-SUP-CA
           CALL "APSUPVAL" USING WS-SUP-CA
           MOVE "new purchasing is suspended" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE SV-CAN-PURCHASE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the existing liability is untouched" TO TK-NAME
           MOVE 1200.00 TO TK-EXPECTED-N
           MOVE SV-OPEN-BAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the payment hold is not set with it" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE SV-PAYMENT-HOLD TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "legitimate payment is still allowed" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SV-CAN-PAY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "it is a warning, not a bar on the account"
               TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-SUP-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-SUP-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the purchasing suspension is named" TO TK-NAME
           MOVE "APS0007" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-SUP-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0400-SUPPLIER-HEADER
           MOVE "ZZAPGAP-PAY1" TO CA-CORRELATION-ID OF WS-SUP-CA
           MOVE "CANPAY" TO CA-FUNCTION OF WS-SUP-CA
           CALL "APSUPVAL" USING WS-SUP-CA
           MOVE "the payment gate still says yes" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-SUP-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-SUP-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

      *>   AP-001 - a hold is a decision and must carry its reason
       1200-HOLD-NEEDS-A-REASON.
           PERFORM 0400-SUPPLIER-HEADER
           MOVE "ZZAPGAP-HOLD2" TO CA-CORRELATION-ID OF WS-SUP-CA
           MOVE "HOLD" TO CA-FUNCTION OF WS-SUP-CA
           MOVE "PAYMENT" TO SV-HOLD-TYPE
           MOVE SPACES TO SV-REASON
           CALL "APSUPVAL" USING WS-SUP-CA
           MOVE "a hold without a reason is refused" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-SUP-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-SUP-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the missing reason is named" TO TK-NAME
           MOVE "APS0004" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-SUP-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AP-001 / AC-028 - stopping payment is a second, separate
      *>   policy; lifting it leaves the purchasing suspension alone
       1300-PAYMENT-HOLD-IS-SEPARATE.
           PERFORM 0400-SUPPLIER-HEADER
           MOVE "ZZAPGAP-HOLD3" TO CA-CORRELATION-ID OF WS-SUP-CA
           MOVE "HOLD" TO CA-FUNCTION OF WS-SUP-CA
           MOVE "PAYMENT" TO SV-HOLD-TYPE
           MOVE "Supplier bank details under investigation"
               TO SV-REASON
           CALL "APSUPVAL" USING WS-SUP-CA
           MOVE "a payment hold is applied separately" TO TK-NAME
           MOVE CA-MESSAGE OF WS-SUP-CA TO TK-DETAIL
           IF CA-RETURN-CODE OF WS-SUP-CA < 8
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           MOVE "both policies now restrict the account" TO TK-NAME
           IF SV-PURCHASE-HOLD = "Y" AND SV-PAYMENT-HOLD = "Y"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           PERFORM 0400-SUPPLIER-HEADER
           MOVE "ZZAPGAP-PAY2" TO CA-CORRELATION-ID OF WS-SUP-CA
           MOVE "CANPAY" TO CA-FUNCTION OF WS-SUP-CA
           CALL "APSUPVAL" USING WS-SUP-CA
           MOVE "payment is now held for authority" TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-SUP-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-SUP-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the payment hold is named" TO TK-NAME
           MOVE "APS0009" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-SUP-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the debt is still reported while held" TO TK-NAME
           MOVE 1200.00 TO TK-EXPECTED-N
           MOVE SV-OPEN-BAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0400-SUPPLIER-HEADER
           MOVE "ZZAPGAP-REL1" TO CA-CORRELATION-ID OF WS-SUP-CA
           MOVE "RELEASE" TO CA-FUNCTION OF WS-SUP-CA
           MOVE "PAYMENT" TO SV-HOLD-TYPE
           MOVE "Bank details confirmed" TO SV-REASON
           CALL "APSUPVAL" USING WS-SUP-CA
           PERFORM 0400-SUPPLIER-HEADER
           MOVE "ZZAPGAP-VAL3" TO CA-CORRELATION-ID OF WS-SUP-CA
           MOVE "VALIDATE" TO CA-FUNCTION OF WS-SUP-CA
           CALL "APSUPVAL" USING WS-SUP-CA
           MOVE "lifting the payment hold restores payment"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SV-CAN-PAY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "and the purchasing suspension survives it"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SV-PURCHASE-HOLD TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0400-SUPPLIER-HEADER
           MOVE "ZZAPGAP-REL2" TO CA-CORRELATION-ID OF WS-SUP-CA
           MOVE "RELEASE" TO CA-FUNCTION OF WS-SUP-CA
           MOVE "PURCHASE" TO SV-HOLD-TYPE
           MOVE "Quality investigation closed" TO SV-REASON
           CALL "APSUPVAL" USING WS-SUP-CA
           MOVE "the purchasing suspension can be lifted too"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SV-CAN-PURCHASE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AP-009 / AC-031 - the due date is derived from the recorded
      *>   terms and the supplier's own document date.  The document
      *>   reached us on 5 October but D2N30 still falls due thirty
      *>   days after 10 September, with the two per cent discount
      *>   deadline ten days after it.
       1400-DUE-DATE-FROM-TERMS.
           PERFORM 0500-BLOCK-HEADER
           MOVE "ZZAPGAP-DUE1" TO CA-CORRELATION-ID OF WS-BLK-CA
           MOVE "DUEDATE" TO CA-FUNCTION OF WS-BLK-CA
           MOVE "D2N30" TO BK-TERMS-CODE
           MOVE 20260910 TO BK-DOCUMENT-DATE
           MOVE 20261005 TO BK-DATE-RECEIVED
           MOVE 1200.00 TO BK-AMOUNT-GBP
           CALL "APBLOCK" USING WS-BLK-CA
           MOVE "the derivation is clean" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-BLK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-BLK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "thirty days from 10 September is 10 October"
               TO TK-NAME
           MOVE 20261010 TO TK-EXPECTED-N
           MOVE BK-DUE-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the late internal receipt does not move it"
               TO TK-NAME
           MOVE 20261010 TO TK-EXPECTED-N
           MOVE BK-DUE-DATE-RAW TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the discount deadline is 20 September" TO TK-NAME
           MOVE 20260920 TO TK-EXPECTED-N
           MOVE BK-DISCOUNT-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "two per cent of 1,200.00 is 24.00" TO TK-NAME
           MOVE 24.00 TO TK-EXPECTED-N
           MOVE BK-DISCOUNT-AMT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the term length is thirty days" TO TK-NAME
           MOVE 30 TO TK-EXPECTED-N
           MOVE BK-TERM-DAYS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the dates come from the recorded terms" TO TK-NAME
           MOVE "TERMS" TO TK-EXPECTED-X
           MOVE BK-TERMS-SOURCE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the internal delay is noted, not acted on"
               TO TK-NAME
           IF BK-NOTE NOT = SPACES
               MOVE BK-NOTE TO TK-DETAIL
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>   AP-009 - an override is allowed, but it is recorded and the
      *>   contractual date is kept alongside it
       1450-DUE-DATE-OVERRIDE.
           PERFORM 0500-BLOCK-HEADER
           MOVE "ZZAPGAP-DUE2" TO CA-CORRELATION-ID OF WS-BLK-CA
           MOVE "DUEDATE" TO CA-FUNCTION OF WS-BLK-CA
           MOVE "D2N30" TO BK-TERMS-CODE
           MOVE 20260910 TO BK-DOCUMENT-DATE
           MOVE 1200.00 TO BK-AMOUNT-GBP
           MOVE "Agreed extension signed by the buyer"
               TO BK-OVERRIDE-REASON
           CALL "APBLOCK" USING WS-BLK-CA
           MOVE "an authorised override is a warning" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-BLK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-BLK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the override is given its own reason code"
               TO TK-NAME
           MOVE "APK0012" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-BLK-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the contractual date is still retained" TO TK-NAME
           MOVE 20261010 TO TK-EXPECTED-N
           MOVE BK-DUE-DATE-RAW TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AP-008 - a block names its amount, reason and owner
       1500-BLOCK-NEEDS-A-REASON.
           PERFORM 0500-BLOCK-HEADER
           MOVE "ZZAPGAP-BLK0" TO CA-CORRELATION-ID OF WS-BLK-CA
           MOVE "BLOCK" TO CA-FUNCTION OF WS-BLK-CA
           MOVE "ZZAPGREG1" TO BK-REGISTER-NUMBER
           MOVE SPACES TO BK-REASON
           CALL "APBLOCK" USING WS-BLK-CA
           MOVE "a block without a reason is refused" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-BLK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-BLK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the missing reason is named" TO TK-NAME
           MOVE "APK0003" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-BLK-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AP-008 / AC-031 - the liability stays recognised in the
      *>   accounts while payment of it is stopped
       1600-POSTED-BUT-BLOCKED.
           PERFORM 0500-BLOCK-HEADER
           MOVE "ZZAPGAP-BLK1" TO CA-CORRELATION-ID OF WS-BLK-CA
           MOVE "BLOCK" TO CA-FUNCTION OF WS-BLK-CA
           MOVE "ZZAPGREG1" TO BK-REGISTER-NUMBER
           MOVE "SET" TO BK-ACTION
           MOVE "Short delivery of 10 units under query"
               TO BK-REASON
           MOVE "APCLERK" TO BK-OWNER-USER
           MOVE "APMGR" TO BK-OWNER-ROLE
           MOVE 1200.00 TO BK-AMOUNT-GBP
           CALL "APBLOCK" USING WS-BLK-CA
           MOVE "blocking payment is reported as a warning"
               TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-BLK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-BLK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the block quotes the recognised liability rule"
               TO TK-NAME
           MOVE "APK0010" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-BLK-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the open item is blocked for payment" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE BK-IS-BLOCKED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "one open item was affected" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE BK-ITEMS-AFFECTED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the block is owned and quantified" TO TK-NAME
           IF BK-EXCEPTION-ID > 0 AND BK-AMOUNT-GBP = 1200.00
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           PERFORM 8200-READ-INVOICE-STATE
           MOVE "the liability is still recognised as posted"
               TO TK-NAME
           MOVE "POSTED" TO TK-EXPECTED-X
           MOVE WS-R2-ONE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "but it is no longer eligible for payment"
               TO TK-NAME
           MOVE "BLOCKED" TO TK-EXPECTED-X
           MOVE WS-R2-TWO TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AP-008 - an accounting exception is never passed off as a
      *>   clean match: the dispute is recorded against the item and
      *>   the accounting state does not move
       1700-DISPUTE-IS-NOT-A-MATCH.
           PERFORM 0500-BLOCK-HEADER
           MOVE "ZZAPGAP-DSP0" TO CA-CORRELATION-ID OF WS-BLK-CA
           MOVE "DISPUTE" TO CA-FUNCTION OF WS-BLK-CA
           MOVE "ZZAPGREG1" TO BK-REGISTER-NUMBER
           MOVE "SET" TO BK-ACTION
           MOVE SPACES TO BK-REASON
           CALL "APBLOCK" USING WS-BLK-CA
           MOVE "a dispute without a reason is refused" TO TK-NAME
           MOVE "APK0005" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-BLK-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-BLK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           PERFORM 0500-BLOCK-HEADER
           MOVE "ZZAPGAP-DSP1" TO CA-CORRELATION-ID OF WS-BLK-CA
           MOVE "DISPUTE" TO CA-FUNCTION OF WS-BLK-CA
           MOVE "ZZAPGREG1" TO BK-REGISTER-NUMBER
           MOVE "SET" TO BK-ACTION
           MOVE "Price charged is 8.40, the order says 8.00"
               TO BK-REASON
           MOVE "APCLERK" TO BK-OWNER-USER
           MOVE 1200.00 TO BK-AMOUNT-GBP
           CALL "APBLOCK" USING WS-BLK-CA
           MOVE "a match dispute is recorded and owned" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-BLK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-BLK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the dispute quotes its own rule" TO TK-NAME
           MOVE "APK0011" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-BLK-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the item is marked as disputed" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE BK-IS-DISPUTED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8300-READ-ITEM-FLAGS
           MOVE "the open item carries the dispute" TO TK-NAME
           MOVE "1" TO TK-EXPECTED-X
           MOVE WS-R2-ONE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "and the payment block stands beside it" TO TK-NAME
           MOVE "1" TO TK-EXPECTED-X
           MOVE WS-R2-TWO TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8200-READ-INVOICE-STATE
           MOVE "the accounting state is still posted" TO TK-NAME
           MOVE "POSTED" TO TK-EXPECTED-X
           MOVE WS-R2-ONE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AP-008 - the resolution is recorded when the block is
      *>   lifted, so every block has an outcome
       1800-RELEASE-NEEDS-OUTCOME.
           PERFORM 0500-BLOCK-HEADER
           MOVE "ZZAPGAP-REL3" TO CA-CORRELATION-ID OF WS-BLK-CA
           MOVE "RELEASE" TO CA-FUNCTION OF WS-BLK-CA
           MOVE "ZZAPGREG1" TO BK-REGISTER-NUMBER
           MOVE SPACES TO BK-RESOLUTION
           CALL "APBLOCK" USING WS-BLK-CA
           MOVE "releasing without a resolution is refused"
               TO TK-NAME
           MOVE "APK0004" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-BLK-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-BLK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           PERFORM 0500-BLOCK-HEADER
           MOVE "ZZAPGAP-REL4" TO CA-CORRELATION-ID OF WS-BLK-CA
           MOVE "RELEASE" TO CA-FUNCTION OF WS-BLK-CA
           MOVE "ZZAPGREG1" TO BK-REGISTER-NUMBER
           MOVE "Credit note received for the short delivery"
               TO BK-RESOLUTION
           CALL "APBLOCK" USING WS-BLK-CA
           MOVE "a recorded resolution releases the block"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-BLK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-BLK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           PERFORM 8200-READ-INVOICE-STATE
           MOVE "the invoice becomes payable again" TO TK-NAME
           MOVE "APPROVED" TO TK-EXPECTED-X
           MOVE WS-R2-TWO TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "and the accounting state never moved" TO TK-NAME
           MOVE "POSTED" TO TK-EXPECTED-X
           MOVE WS-R2-ONE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AP-019 / AC-037 - the supplier's statement of 1,300.00
      *>   against our ledger of 800.00: one line agrees, one is a
      *>   blocked debt, one is a payment in flight and one is an
      *>   unrecorded 50.00 charge
       1900-BUILD-STATEMENT.
           INITIALIZE WS-STM-CA
           MOVE "STC" TO CA-COMPANY OF WS-STM-CA
           MOVE "RDG" TO CA-SITE OF WS-STM-CA
           MOVE "APCLERK" TO CA-USER OF WS-STM-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-STM-CA
           MOVE "ZZAPG2" TO SR-SUPPLIER
           MOVE "ZZAPGAPSTM1" TO SR-STATEMENT-REF
           MOVE 20261010 TO SR-STATEMENT-DATE
           MOVE "GBP" TO SR-CURRENCY
           MOVE 1300.00 TO SR-STATEMENT-BAL
           MOVE "APCLERK" TO SR-OWNER-USER
           MOVE 4 TO SR-LINE-CNT
           MOVE 1 TO SR-L-NUM(1)
           MOVE "ZZAPG/S/100" TO SR-L-REF(1)
           MOVE 20260901 TO SR-L-DATE(1)
           MOVE 20261001 TO SR-L-DUE(1)
           MOVE 500.00 TO SR-L-AMOUNT(1)
           MOVE 2 TO SR-L-NUM(2)
           MOVE "ZZAPG/S/200" TO SR-L-REF(2)
           MOVE 20260902 TO SR-L-DATE(2)
           MOVE 20261002 TO SR-L-DUE(2)
           MOVE 350.00 TO SR-L-AMOUNT(2)
           MOVE 3 TO SR-L-NUM(3)
           MOVE "ZZAPG/S/300" TO SR-L-REF(3)
           MOVE 20260903 TO SR-L-DATE(3)
           MOVE 20261003 TO SR-L-DUE(3)
           MOVE 400.00 TO SR-L-AMOUNT(3)
           MOVE 4 TO SR-L-NUM(4)
           MOVE "ZZAPG/S/900" TO SR-L-REF(4)
           MOVE 20260930 TO SR-L-DATE(4)
           MOVE 20261030 TO SR-L-DUE(4)
           MOVE 50.00 TO SR-L-AMOUNT(4).

       2000-STATEMENT-LOADED.
           PERFORM 1900-BUILD-STATEMENT
           MOVE "ZZAPGAP-STM1" TO CA-CORRELATION-ID OF WS-STM-CA
           MOVE "LOAD" TO CA-FUNCTION OF WS-STM-CA
           CALL "APSTMREC" USING WS-STM-CA
           MOVE "the supplier statement is recorded" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-STM-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-STM-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "it is given a statement number" TO TK-NAME
           IF SR-STATEMENT-ID > 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

       2100-STATEMENT-RECONCILED.
           PERFORM 1900-BUILD-STATEMENT
           MOVE "ZZAPGAP-STM2" TO CA-CORRELATION-ID OF WS-STM-CA
           MOVE "RECONCIL" TO CA-FUNCTION OF WS-STM-CA
           CALL "APSTMREC" USING WS-STM-CA
           MOVE "our ledger shows 800.00 open at that date"
               TO TK-NAME
           MOVE 800.00 TO TK-EXPECTED-N
           MOVE SR-LEDGER-BAL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the statement is 500.00 higher than our ledger"
               TO TK-NAME
           MOVE 500.00 TO TK-EXPECTED-N
           MOVE SR-DIFFERENCE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the agreeing line is matched" TO TK-NAME
           MOVE "MATCHED" TO TK-EXPECTED-X
           MOVE SR-L-STATUS(1) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the blocked debt is reported as disputed"
               TO TK-NAME
           MOVE "DISPUTED" TO TK-EXPECTED-X
           MOVE SR-L-STATUS(2) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the payment in flight is a timing difference"
               TO TK-NAME
           MOVE "TIMING" TO TK-EXPECTED-X
           MOVE SR-L-STATUS(3) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the 50.00 charge is missing from our ledger"
               TO TK-NAME
           MOVE "MISSINGOURS" TO TK-EXPECTED-X
           MOVE SR-L-STATUS(4) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "400.00 of it is our payment in flight" TO TK-NAME
           MOVE 400.00 TO TK-EXPECTED-N
           MOVE SR-TIMING-DIFF TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "50.00 of it is the unrecorded charge" TO TK-NAME
           MOVE 50.00 TO TK-EXPECTED-N
           MOVE SR-MISSING-OURS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "50.00 of it is the blocked debt" TO TK-NAME
           MOVE 50.00 TO TK-EXPECTED-N
           MOVE SR-DISPUTED-AMT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "every penny of the difference is explained"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SR-UNEXPLAINED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "one line agreed and three need review" TO TK-NAME
           IF SR-MATCHED-CNT = 1 AND SR-EXCEPTION-CNT = 3
               PERFORM TK-PASS
           ELSE
               MOVE SR-L-NOTE(4) TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "the differences are a review, not an error"
               TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-STM-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-STM-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the review rule is quoted" TO TK-NAME
           MOVE "APQ0010" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-STM-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the statement is left in dispute, not agreed"
               TO TK-NAME
           MOVE "DISPUTED" TO TK-EXPECTED-X
           MOVE SR-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AP-019 - reconciling reports; it does not post and it does
      *>   not pay.  The ledger is exactly as it was.
       2200-NOTHING-WAS-POSTED.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(40)), "
                  "CAST(ISNULL(SUM(ResidualAmount),0) AS "
                  "varchar(40)) FROM ap.OpenItem WHERE CompanyCode = "
                  "'STC' AND SupplierCode = 'ZZAPG2'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8100-READ-TWO
           MOVE "the unmatched charge created no open item"
               TO TK-NAME
           MOVE 3 TO TK-EXPECTED-N
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R2-ONE)
           MOVE WS-NUM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "and the ledger still stands at 800.00" TO TK-NAME
           MOVE 800.00 TO TK-EXPECTED-N
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R2-TWO)
           MOVE WS-NUM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(40)), '' FROM "
                  "core.ExceptionItem WHERE CompanyCode = 'STC' AND "
                  "DocumentRef = 'ZZAPGAPSTM1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8100-READ-TWO
           MOVE "the unmatched line is owned as an exception"
               TO TK-NAME
           IF FUNCTION NUMVAL(WS-R2-ONE) > 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

       8000-EXEC-SQL.
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW2
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW2.

       8100-READ-TWO.
           MOVE SPACES TO WS-ROW2
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW2.

       8200-READ-INVOICE-STATE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT AccountingState, PaymentEligibility FROM "
                  "ap.PurchaseInvoice WHERE CompanyCode = 'STC' AND "
                  "RegisterNumber = 'ZZAPGREG1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8100-READ-TWO.

       8300-READ-ITEM-FLAGS.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(IsDisputed AS char(1)), "
                  "CAST(IsBlocked AS char(1)) FROM ap.OpenItem WHERE "
                  "CompanyCode = 'STC' AND ItemType = 'INVOICE' AND "
                  "DocumentNumber = 'ZZAPG/INV/1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8100-READ-TWO.

       COPY TESTKITP.
