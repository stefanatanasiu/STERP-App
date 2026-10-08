      *>================================================================
      *> PROGRAM-ID : TAPGAP2
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (AP) UNIT TESTS
      *> PURPOSE    : THE FOUR PLACES WHERE A PURCHASE LEDGER AMOUNT
      *>              MUST KEEP ITS OWN ECONOMIC MEANING.
      *>                APCRDLNK - A SUPPLIER CREDIT DOES NOT MOVE
      *>                  STOCK AND A STOCK RETURN DOES NOT SETTLE AP
      *>                  (AP-010, AC-032); AN ADVANCE IS AN ASSET
      *>                  UNTIL IT IS VALIDLY APPLIED AND A DEBIT
      *>                  BALANCE IS NOT AN AUTOMATIC NEGATIVE PAYMENT
      *>                  (AP-017, AC-036)
      *>                APALLOC  - A FOREIGN SETTLEMENT KEEPS THE
      *>                  ORIGINAL AMOUNT AND THE CARRYING VALUE AND
      *>                  REPORTS THE DIFFERENCE AS A REALISED
      *>                  EXCHANGE GAIN OR LOSS, NEVER AS A PURCHASE
      *>                  PRICE OR SETTLEMENT DIFFERENCE (AP-018,
      *>                  AC-036)
      *>                APINVLIN / APVATEVD - A REVERSE CHARGE INVOICE
      *>                  IS PAID NET AND THE VAT IS SELF ACCOUNTED IN
      *>                  BOTH BOXES; NON RECOVERABLE INPUT TAX IS
      *>                  ACCOUNTED SEPARATELY (AP-021, AC-038)
      *>                APBANKVR - A PAYEE CHANGE ASKED FOR BY EMAIL
      *>                  NEEDS CONFIRMATION OF PAYEE, AN INDEPENDENT
      *>                  CALL-BACK TO A CONTACT ALREADY ON RECORD AND
      *>                  AN INDEPENDENT APPROVER (AP-027, AC-127)
      *>                APLATEPY - A STATUTORY CLAIM IS A FINANCE
      *>                  COST, AND TERMS BEYOND THE MAXIMUM FOR A
      *>                  SMALL SUPPLIER NEED AN EXCEPTION (AP-030,
      *>                  AC-128)
      *>              EVERY FIXTURE CARRIES THE PREFIX ZZAPH AND IS
      *>              REMOVED BEFORE AND AFTER THE RUN.  WITH NO
      *>              DATABASE THE SUITE SKIPS RATHER THAN FAILS.
      *>              THE LEDGER CALLS DELIBERATELY RUN WITHOUT A
      *>              CORRELATION ID SO THAT NO NOMINAL LEDGER
      *>              JOURNAL IS RAISED BY A UNIT TEST; THE BANK
      *>              DETAIL CHANGE, WHICH IS IDEMPOTENT THROUGH
      *>              core.Approval, CARRIES A UNIQUE ONE.
      *>----------------------------------------------------------------
      *> 2026-10-07 STERP ORIGINAL
      *>================================================================
      *> REQ: AP-010, AP-017, AP-018, AP-021, AP-027, AP-030
      *> AC: AC-032, AC-036, AC-038, AC-127, AC-128
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAPGAP2.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY APLINWRK.
       COPY APCRDLNKCA REPLACING ==DFHCOMMAREA== BY ==WS-CRD-CA==.
       COPY APALLOCCA REPLACING ==DFHCOMMAREA== BY ==WS-ALL-CA==.
       COPY APVATEVDCA REPLACING ==DFHCOMMAREA== BY ==WS-VAT-CA==.
       COPY APBANKVRCA REPLACING ==DFHCOMMAREA== BY ==WS-BNK-CA==.
       COPY APLATEPYCA REPLACING ==DFHCOMMAREA== BY ==WS-LTE-CA==.
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
           05  WS-TARGET-ID            PIC S9(9) COMP-5 VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TAPGAP2 credits, FX, VAT, payees" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-EVERYTHING
           ELSE
               PERFORM 0200-CLEAN-UP
               PERFORM 0300-MAKE-SUPPLIER
               PERFORM 0310-MAKE-OPEN-ITEMS
               PERFORM 0320-MAKE-RETURN
               PERFORM 1000-RETURN-STILL-UNCREDITED
               PERFORM 1100-CREDIT-MOVES-NO-STOCK
               PERFORM 1200-CREDIT-OVER-THE-RETURN
               PERFORM 1300-ADVANCE-IS-AN-ASSET
               PERFORM 1400-ADVANCE-CLEARS-ONCE
               PERFORM 1500-REFUND-IS-NOT-AN-ADVANCE
               PERFORM 1550-SETOFF-NEEDS-AGREEMENT
               PERFORM 1600-FX-ON-SETTLEMENT
               PERFORM 1700-FX-GAIN-AND-PARITY
               PERFORM 1800-FX-IS-NOT-A-VARIANCE
               PERFORM 2000-REVERSE-CHARGE-LINE
               PERFORM 2100-NON-RECOVERABLE-VAT
               PERFORM 2200-REVERSE-CHARGE-BOXES
               PERFORM 2300-IMPORT-VAT-EVIDENCE
               PERFORM 2400-EMAIL-ALONE-CANNOT-PAY
               PERFORM 2500-COP-NEEDS-A-DECISION
               PERFORM 2600-CALLBACK-MUST-BE-ON-FILE
               PERFORM 2700-APPROVAL-IS-INDEPENDENT
               PERFORM 2800-LATE-PAYMENT-IS-FINANCE-COST
               PERFORM 2900-TERMS-AGAINST-THE-LIMIT
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
           MOVE 6 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "N" TO WS-DB-UP
           END-IF.

       0900-SKIP-EVERYTHING.
           MOVE "credits, advances, exchange, VAT and payees"
               TO TK-NAME
           MOVE "the purchase ledger tables are not deployed"
               TO TK-DETAIL
           PERFORM TK-SKIP.

       0200-CLEAN-UP.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; DELETE po.SupplierReturnLine "
                  "WHERE SupplierReturnId IN (SELECT "
                  "SupplierReturnId FROM po.SupplierReturn WHERE "
                  "CompanyCode = 'STC' AND ReturnRef LIKE 'ZZAPH%'); "
                  "DELETE po.SupplierReturn WHERE CompanyCode = "
                  "'STC' AND ReturnRef LIKE 'ZZAPH%'; "
                  "DELETE ap.Allocation WHERE CompanyCode = 'STC' "
                  "AND SupplierCode LIKE 'ZZAPH%'; "
                  "DELETE ap.OpenItem WHERE CompanyCode = 'STC' AND "
                  "SupplierCode LIKE 'ZZAPH%'; "
                  "DELETE core.Approval WHERE CompanyCode = 'STC' "
                  "AND DocumentRef LIKE 'ZZAPH%'; "
                  "DELETE core.IdempotencyKey WHERE CorrelationId "
                  "LIKE 'ZZAPH%'; "
                  "DELETE core.AuditLog WHERE CompanyCode = 'STC' "
                  "AND EntityKey LIKE 'ZZAPH%'; "
                  "DELETE ap.SupplierBank WHERE CompanyCode = 'STC' "
                  "AND SupplierCode LIKE 'ZZAPH%'; "
                  "DELETE ap.Supplier WHERE CompanyCode = 'STC' AND "
                  "SupplierCode LIKE 'ZZAPH%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8000-EXEC-SQL.

       0300-MAKE-SUPPLIER.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT ap.Supplier (CompanyCode, "
                  "SupplierCode, LegalName, TermsCode, CurrencyCode, "
                  "PaymentMethod, Status, BusinessSize, CreatedBy) "
                  "VALUES ('STC','ZZAPH1','Gap Test Precision Ltd',"
                  "'N30','GBP','BACS','ACTIVE','SMALL','TEST'); "
                  "INSERT ap.SupplierBank (CompanyCode, "
                  "SupplierCode, BankRef, BeneficiaryName, SortCode, "
                  "AccountNumber, Status, PreparedBy, ApprovedBy, "
                  "ApprovedAt, CreatedBy) VALUES ('STC','ZZAPH1',"
                  "'MAIN','Gap Test Precision Ltd','201234',"
                  "'41234567','APPROVED','APMGR','TREASURY',"
                  "SYSDATETIME(),'TEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8000-EXEC-SQL.

      *>   the 100.00 invoice of AC-036, and the US$1,200 liability
      *>   carried at US$1.25 = GBP 1 with the payment that settles it
       0310-MAKE-OPEN-ITEMS.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT ap.OpenItem (CompanyCode, "
                  "SupplierCode, ItemType, DocumentNumber, "
                  "DocumentDate, DueDate, PostingDate, CurrencyCode, "
                  "FxRate, OriginalAmount, OriginalAmountGBP, "
                  "ResidualAmount, ResidualAmountGBP, TermsCode, "
                  "SettlementStatus, CreatedBy) VALUES "
                  "('STC','ZZAPH1','INVOICE','ZZAPH/INV/A',"
                  "'2026-09-15','2026-10-15','2026-09-15','GBP',1,"
                  "100.00,100.00,100.00,100.00,'N30','OPEN','TEST'),"
                  " ('STC','ZZAPH1','INVOICE','ZZAPH/INV/U',"
                  "'2026-09-20','2026-10-20','2026-09-20','USD',"
                  "1.25,1200.00,960.00,1200.00,960.00,'N30','OPEN',"
                  "'TEST'), ('STC','ZZAPH1','PAYMENT','ZZAPH/PAY/U',"
                  "'2026-10-15','2026-10-15','2026-10-15','USD',"
                  "1.25,-1200.00,-960.00,-1200.00,-960.00,NULL,"
                  "'OPEN','TEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8000-EXEC-SQL.

      *>   ten units returned after invoicing, 250.00 of value still
      *>   sitting in supplier return clearing
       0320-MAKE-RETURN.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT po.SupplierReturn "
                  "(CompanyCode, SiteCode, ReturnRef, SupplierCode, "
                  "ReturnDate, PostingDate, ReasonCode, "
                  "IsPostInvoice, ExpectedCreditGBP, Status, "
                  "CreatedBy) VALUES ('STC','RDG','ZZAPHRET1',"
                  "'ZZAPH1','2026-10-01','2026-10-01','DAMAGED',1,"
                  "250.00,'OPEN','TEST'); "
                  "INSERT po.SupplierReturnLine (SupplierReturnId, "
                  "LineNum, ItemCode, ReturnedQty, StockQty, "
                  "CreditedQty, ReturnValueGBP, ClearingValueGBP) "
                  "SELECT r.SupplierReturnId, 1, 'ZZAPH-ITEM', 10, "
                  "10, 0, 250.00, 250.00 FROM po.SupplierReturn r "
                  "WHERE r.CompanyCode = 'STC' AND r.ReturnRef = "
                  "'ZZAPHRET1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8000-EXEC-SQL.

       0400-CREDIT-HEADER.
           INITIALIZE WS-CRD-CA
           MOVE "STC" TO CA-COMPANY OF WS-CRD-CA
           MOVE "RDG" TO CA-SITE OF WS-CRD-CA
           MOVE "APCLERK" TO CA-USER OF WS-CRD-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-CRD-CA
           MOVE "ZZAPH1" TO CL-SUPPLIER.

       0500-ALLOC-HEADER.
           INITIALIZE WS-ALL-CA
           MOVE "STC" TO CA-COMPANY OF WS-ALL-CA
           MOVE "RDG" TO CA-SITE OF WS-ALL-CA
           MOVE "APCLERK" TO CA-USER OF WS-ALL-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-ALL-CA
           MOVE "ZZAPH1" TO AL-SUPPLIER
           MOVE 20261015 TO AL-EFFECTIVE-DATE
           MOVE 1.00 TO AL-DIFF-TOLERANCE.

       0600-VAT-HEADER.
           INITIALIZE WS-VAT-CA
           MOVE "STC" TO CA-COMPANY OF WS-VAT-CA
           MOVE "RDG" TO CA-SITE OF WS-VAT-CA
           MOVE "VATOWNER" TO CA-USER OF WS-VAT-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-VAT-CA
           MOVE "ZZAPH1" TO VE-SUPPLIER
           MOVE 6 TO VE-REPAY-MONTHS.

       0700-BANK-HEADER.
           INITIALIZE WS-BNK-CA
           MOVE "STC" TO CA-COMPANY OF WS-BNK-CA
           MOVE "RDG" TO CA-SITE OF WS-BNK-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-BNK-CA
           MOVE "ZZAPH1" TO BV-SUPPLIER
           MOVE "NEWBK" TO BV-BANK-REF.

       0800-LATE-HEADER.
           INITIALIZE WS-LTE-CA
           MOVE "STC" TO CA-COMPANY OF WS-LTE-CA
           MOVE "RDG" TO CA-SITE OF WS-LTE-CA
           MOVE "APMGR" TO CA-USER OF WS-LTE-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-LTE-CA
           MOVE "ZZAPH1" TO LP-SUPPLIER
           MOVE "LPCDIA1998" TO LP-STATUTE-REF
           MOVE 8.0000 TO LP-MARGIN-PCT
           MOVE 40.00 TO LP-BAND1-GBP
           MOVE 70.00 TO LP-BAND2-GBP
           MOVE 100.00 TO LP-BAND3-GBP
           MOVE 60 TO LP-MAX-TERM-DAYS.

       8000-EXEC-SQL.
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 6 TO SQLIO-CURSOR
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
           MOVE 6 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW2.

       8200-READ-ADVANCE-ROW.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ItemType, CAST(ResidualAmountGBP AS "
                  "varchar(40)) FROM ap.OpenItem WHERE CompanyCode = "
                  "'STC' AND DocumentNumber = 'ZZAPH/ADV/1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8100-READ-TWO.

       8300-READ-TARGET-ROW.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(OpenItemId AS varchar(40)), "
                  "CAST(ResidualAmountGBP AS varchar(40)) FROM "
                  "ap.OpenItem WHERE CompanyCode = 'STC' AND "
                  "ItemType = 'INVOICE' AND DocumentNumber = "
                  "'ZZAPH/INV/A'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8100-READ-TWO.

      *>   AP-010 / AC-032 - ten units went back to the supplier after
      *>   the invoice was recognised.  The physical return did not
      *>   settle the payable: its value waits in return clearing.
       1000-RETURN-STILL-UNCREDITED.
           PERFORM 0400-CREDIT-HEADER
           MOVE "RETSTAT" TO CA-FUNCTION OF WS-CRD-CA
           MOVE "ZZAPHRET1" TO CL-RETURN-REF
           CALL "APCRDLNK" USING WS-CRD-CA
           MOVE "the return detail is found" TO TK-NAME
           IF CA-RETURN-CODE OF WS-CRD-CA < 8
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-CRD-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "ten units were returned" TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE CL-RETURNED-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "none of them has been credited yet" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CL-CREDITED-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "ten units remain to be credited" TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE CL-REMAINING-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "250.00 of value sits in return clearing" TO TK-NAME
           MOVE 250.00 TO TK-EXPECTED-N
           MOVE CL-EXPECTED-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "and all 250.00 of it is still uncredited"
               TO TK-NAME
           MOVE 250.00 TO TK-EXPECTED-N
           MOVE CL-REMAINING-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the returned item is identified" TO TK-NAME
           MOVE "ZZAPH-ITEM" TO TK-EXPECTED-X
           MOVE CL-ITEM TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8300-READ-TARGET-ROW
           MOVE "the return has settled nothing in the ledger"
               TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R2-TWO)
           MOVE WS-NUM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AP-010 / AC-032 - the credit note is linked to the return,
      *>   but the two stay separate events: the credit changes AP and
      *>   tax, the return already changed the quantity.
       1100-CREDIT-MOVES-NO-STOCK.
           PERFORM 0400-CREDIT-HEADER
           MOVE "CRDLINK" TO CA-FUNCTION OF WS-CRD-CA
           MOVE SPACES TO CL-RETURN-REF
           CALL "APCRDLNK" USING WS-CRD-CA
           MOVE "a credit cannot be linked to no return" TO TK-NAME
           MOVE "APC0002" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CRD-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-CRD-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           PERFORM 0400-CREDIT-HEADER
           MOVE "CRDLINK" TO CA-FUNCTION OF WS-CRD-CA
           MOVE "ZZAPHRET1" TO CL-RETURN-REF
           MOVE 100.00 TO CL-CREDITED-GBP
           CALL "APCRDLNK" USING WS-CRD-CA
           MOVE "the credit is linked to the return" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CRD-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CRD-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the link quotes the return clearing rule" TO TK-NAME
           MOVE "APC0011" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CRD-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a supplier credit moves no stock" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE CL-STOCK-AFFECTED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "what is not credited stays in return clearing"
               TO TK-NAME
           IF CL-CLEARING-GBP = CL-REMAINING-GBP
              AND CL-CLEARING-GBP > 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           PERFORM 8300-READ-TARGET-ROW
           MOVE "linking the credit settled no payable" TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R2-TWO)
           MOVE WS-NUM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AP-010 - a credit larger than the uncredited return value is
      *>   held for authority, never quietly accepted
       1200-CREDIT-OVER-THE-RETURN.
           PERFORM 0400-CREDIT-HEADER
           MOVE "CRDLINK" TO CA-FUNCTION OF WS-CRD-CA
           MOVE "ZZAPHRET1" TO CL-RETURN-REF
           MOVE 300.00 TO CL-CREDITED-GBP
           CALL "APCRDLNK" USING WS-CRD-CA
           MOVE "a credit beyond the return is held" TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CRD-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CRD-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the over-credit is named" TO TK-NAME
           MOVE "APC0010" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CRD-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AP-017 / AC-036 - a 20.00 advance is a prepayment asset, not
      *>   a reduction of the payable and not a payment of it
       1300-ADVANCE-IS-AN-ASSET.
           PERFORM 0400-CREDIT-HEADER
           MOVE "ADVANCE" TO CA-FUNCTION OF WS-CRD-CA
           MOVE "ZZAPH/ADV/1" TO CL-ADVANCE-NUMBER
           MOVE "GBP" TO CL-ADVANCE-CCY
           MOVE 20.00 TO CL-ADVANCE-AMT
           MOVE 20261015 TO CL-EFFECTIVE-DATE
           CALL "APCRDLNK" USING WS-CRD-CA
           MOVE "the advance is recorded" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CRD-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CRD-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "it is described as a prepayment asset" TO TK-NAME
           MOVE "APC0013" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CRD-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the whole 20.00 is still unapplied" TO TK-NAME
           MOVE 20.00 TO TK-EXPECTED-N
           MOVE CL-ADVANCE-RESID TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 8200-READ-ADVANCE-ROW
           MOVE "the ledger keeps it as an advance, not a payment"
               TO TK-NAME
           MOVE "ADVANCE" TO TK-EXPECTED-X
           MOVE WS-R2-ONE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "it stands as a 20.00 debit to the supplier"
               TO TK-NAME
           MOVE -20.00 TO TK-EXPECTED-N
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R2-TWO)
           MOVE WS-NUM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AP-017 / AC-036 - applying the advance to the 100.00 invoice
      *>   leaves 80.00 to pay and the advance clears exactly once
       1400-ADVANCE-CLEARS-ONCE.
           PERFORM 8300-READ-TARGET-ROW
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R2-ONE)
           MOVE WS-NUM TO WS-TARGET-ID
           PERFORM 0400-CREDIT-HEADER
           MOVE "APPLYADV" TO CA-FUNCTION OF WS-CRD-CA
           MOVE "ZZAPH/ADV/1" TO CL-ADVANCE-NUMBER
           MOVE WS-TARGET-ID TO CL-TARGET-ID
           MOVE 20.00 TO CL-APPLY-AMT
           MOVE 20261015 TO CL-EFFECTIVE-DATE
           CALL "APCRDLNK" USING WS-CRD-CA
           MOVE "the advance is applied cleanly" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CRD-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CRD-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "nothing of the advance is left" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CL-ADVANCE-RESID TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 8300-READ-TARGET-ROW
           MOVE "80.00 of the invoice is left to pay" TO TK-NAME
           MOVE 80.00 TO TK-EXPECTED-N
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R2-TWO)
           MOVE WS-NUM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 8200-READ-ADVANCE-ROW
           MOVE "the advance asset has been used up" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R2-TWO)
           MOVE WS-NUM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0400-CREDIT-HEADER
           MOVE "APPLYADV" TO CA-FUNCTION OF WS-CRD-CA
           MOVE "ZZAPH/ADV/1" TO CL-ADVANCE-NUMBER
           MOVE WS-TARGET-ID TO CL-TARGET-ID
           CALL "APCRDLNK" USING WS-CRD-CA
           MOVE "it cannot be applied a second time" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CRD-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CRD-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the second attempt is named" TO TK-NAME
           MOVE "APC0015" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CRD-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8300-READ-TARGET-ROW
           MOVE "and the expense is not relieved twice" TO TK-NAME
           MOVE 80.00 TO TK-EXPECTED-N
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R2-TWO)
           MOVE WS-NUM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AP-017 - a refund and an application are different events;
      *>   an advance that has been used cannot also be refunded
       1500-REFUND-IS-NOT-AN-ADVANCE.
           PERFORM 0400-CREDIT-HEADER
           MOVE "REFUND" TO CA-FUNCTION OF WS-CRD-CA
           MOVE "ZZAPH/ADV/1" TO CL-ADVANCE-NUMBER
           MOVE SPACES TO CL-REFUND-TYPE
           MOVE 5.00 TO CL-REFUND-AMT
           CALL "APCRDLNK" USING WS-CRD-CA
           MOVE "a refund must say what it refunds" TO TK-NAME
           MOVE "APC0004" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CRD-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-CRD-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           PERFORM 0400-CREDIT-HEADER
           MOVE "REFUND" TO CA-FUNCTION OF WS-CRD-CA
           MOVE "ZZAPH/ADV/1" TO CL-ADVANCE-NUMBER
           MOVE "ADVANCE" TO CL-REFUND-TYPE
           MOVE 5.00 TO CL-REFUND-AMT
           CALL "APCRDLNK" USING WS-CRD-CA
           MOVE "an applied advance cannot be refunded as well"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CRD-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CRD-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the unapplied balance rule is quoted" TO TK-NAME
           MOVE "APC0016" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CRD-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AP-017 - a debit balance never turns itself into a negative
      *>   payment: a set-off needs a recorded legal agreement and an
      *>   authorised approver
       1550-SETOFF-NEEDS-AGREEMENT.
           PERFORM 8300-READ-TARGET-ROW
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R2-ONE)
           MOVE WS-NUM TO WS-TARGET-ID
           PERFORM 0400-CREDIT-HEADER
           MOVE "SETOFF" TO CA-FUNCTION OF WS-CRD-CA
           MOVE "C0001" TO CL-SETOFF-CUSTOMER
           MOVE 50.00 TO CL-SETOFF-AMT
           MOVE "N" TO CL-SETOFF-AGREED
           MOVE WS-TARGET-ID TO CL-TARGET-ID
           CALL "APCRDLNK" USING WS-CRD-CA
           MOVE "a set-off without an agreement is held" TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CRD-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CRD-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the missing right of set-off is named" TO TK-NAME
           MOVE "APC0017" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CRD-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0400-CREDIT-HEADER
           MOVE "SETOFF" TO CA-FUNCTION OF WS-CRD-CA
           MOVE "C0001" TO CL-SETOFF-CUSTOMER
           MOVE 50.00 TO CL-SETOFF-AMT
           MOVE "Y" TO CL-SETOFF-AGREED
           MOVE WS-TARGET-ID TO CL-TARGET-ID
           CALL "APCRDLNK" USING WS-CRD-CA
           MOVE "an agreed set-off still needs authority" TO TK-NAME
           MOVE "APC0018" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CRD-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-CRD-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           PERFORM 0400-CREDIT-HEADER
           MOVE "TREASURY" TO CA-USER OF WS-CRD-CA
           MOVE "SETOFF" TO CA-FUNCTION OF WS-CRD-CA
           MOVE "C0001" TO CL-SETOFF-CUSTOMER
           MOVE 50.00 TO CL-SETOFF-AMT
           MOVE "Y" TO CL-SETOFF-AGREED
           MOVE WS-TARGET-ID TO CL-TARGET-ID
           CALL "APCRDLNK" USING WS-CRD-CA
           MOVE "an agreed and approved set-off is recorded"
               TO TK-NAME
           MOVE "APC0019" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CRD-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-CRD-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

      *>   AP-018 / AC-036 - a US$1,200 liability carried at
      *>   US$1.25 = GBP 1 and settled at US$1.20 = GBP 1.  The cash
      *>   costs 1,000.00, the payable carried 960.00 and the 40.00
      *>   between them is a realised exchange loss.
       1600-FX-ON-SETTLEMENT.
           PERFORM 0500-ALLOC-HEADER
           MOVE "FXREAL" TO CA-FUNCTION OF WS-ALL-CA
           MOVE "PAYMENT" TO AL-SOURCE-TYPE
           MOVE "USD" TO AL-SOURCE-CCY
           MOVE 1200.00 TO AL-SOURCE-AMOUNT
           MOVE 1.25 TO AL-SOURCE-RATE
           MOVE 1.20 TO AL-SETTLE-RATE
           CALL "APALLOC" USING WS-ALL-CA
           MOVE "the original US$1,200 is kept" TO TK-NAME
           MOVE 1200.00 TO TK-EXPECTED-N
           MOVE AL-SOURCE-AMOUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the liability was carried at 960.00" TO TK-NAME
           MOVE 960.00 TO TK-EXPECTED-N
           MOVE AL-APPLIED-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the realised exchange difference is 40.00"
               TO TK-NAME
           MOVE 40.00 TO TK-EXPECTED-N
           MOVE AL-REALISED-FX-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "paying 1,000.00 for a 960.00 debt is a loss"
               TO TK-NAME
           MOVE "LOSS" TO TK-EXPECTED-X
           MOVE AL-FX-POSITION TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "it is not a settlement difference" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE AL-DIFFERENCE-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "and it is not a settlement discount" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE AL-DISCOUNT-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the currency of the debt is retained" TO TK-NAME
           MOVE "USD" TO TK-EXPECTED-X
           MOVE AL-SOURCE-CCY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AP-018 - the other direction is a gain, and a settlement at
      *>   the carrying rate produces no difference at all
       1700-FX-GAIN-AND-PARITY.
           PERFORM 0500-ALLOC-HEADER
           MOVE "FXREAL" TO CA-FUNCTION OF WS-ALL-CA
           MOVE "USD" TO AL-SOURCE-CCY
           MOVE 1200.00 TO AL-SOURCE-AMOUNT
           MOVE 1.25 TO AL-SOURCE-RATE
           MOVE 1.30 TO AL-SETTLE-RATE
           CALL "APALLOC" USING WS-ALL-CA
           MOVE "a stronger pound settles 923.08 against 960.00"
               TO TK-NAME
           MOVE -36.92 TO TK-EXPECTED-N
           MOVE AL-REALISED-FX-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "that difference is a gain" TO TK-NAME
           MOVE "GAIN" TO TK-EXPECTED-X
           MOVE AL-FX-POSITION TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0500-ALLOC-HEADER
           MOVE "FXREAL" TO CA-FUNCTION OF WS-ALL-CA
           MOVE "USD" TO AL-SOURCE-CCY
           MOVE 1200.00 TO AL-SOURCE-AMOUNT
           MOVE 1.25 TO AL-SOURCE-RATE
           MOVE 1.25 TO AL-SETTLE-RATE
           CALL "APALLOC" USING WS-ALL-CA
           MOVE "settling at the carrying rate realises nothing"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE AL-REALISED-FX-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0500-ALLOC-HEADER
           MOVE "FXREAL" TO CA-FUNCTION OF WS-ALL-CA
           MOVE "GBP" TO AL-SOURCE-CCY
           MOVE 1200.00 TO AL-SOURCE-AMOUNT
           MOVE 1.25 TO AL-SOURCE-RATE
           MOVE 1.20 TO AL-SETTLE-RATE
           CALL "APALLOC" USING WS-ALL-CA
           MOVE "a sterling debt has no exchange difference"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE AL-REALISED-FX-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AP-018 / AC-036 - settling the foreign invoice clears the
      *>   960.00 the payable was carried at and reports the 40.00 as
      *>   a realised exchange loss, leaving the settlement difference
      *>   column at nil so that it can never be read as a purchase
      *>   price variance
       1800-FX-IS-NOT-A-VARIANCE.
           PERFORM 0500-ALLOC-HEADER
           MOVE "ALLOCATE" TO CA-FUNCTION OF WS-ALL-CA
           MOVE "PAYMENT" TO AL-SOURCE-TYPE
           MOVE "ZZAPH/PAY/U" TO AL-SOURCE-NUMBER
           MOVE 1.20 TO AL-SETTLE-RATE
           MOVE 1 TO AL-TARGET-CNT
           MOVE "ZZAPH/INV/U" TO AL-T-DOCUMENT(1)
           MOVE 1200.00 TO AL-T-APPLY(1)
           CALL "APALLOC" USING WS-ALL-CA
           MOVE "the settlement is reported as a warning" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-ALL-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-ALL-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "it quotes the realised exchange rule" TO TK-NAME
           MOVE "APZ0010" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-ALL-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the full US$1,200 was applied" TO TK-NAME
           MOVE 1200.00 TO TK-EXPECTED-N
           MOVE AL-APPLIED-TOTAL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "960.00 of carried payable was cleared" TO TK-NAME
           MOVE 960.00 TO TK-EXPECTED-N
           MOVE AL-APPLIED-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "40.00 is held out as a realised exchange loss"
               TO TK-NAME
           MOVE 40.00 TO TK-EXPECTED-N
           MOVE AL-REALISED-FX-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the invoice is settled" TO TK-NAME
           MOVE "SETTLED" TO TK-EXPECTED-X
           MOVE AL-T-STATUS(1) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(a.DifferenceGBP AS varchar(40)), "
                  "CAST(o.ResidualAmountGBP AS varchar(40)) FROM "
                  "ap.Allocation a JOIN ap.OpenItem o ON o.OpenItemId "
                  "= a.TargetOpenItemId WHERE a.CompanyCode = 'STC' "
                  "AND o.DocumentNumber = 'ZZAPH/INV/U'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8100-READ-TWO
           MOVE "nothing is booked as a settlement difference"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R2-ONE)
           MOVE WS-NUM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "and the foreign payable is fully cleared"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R2-TWO)
           MOVE WS-NUM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AP-021 / AC-038 - a 500.00 service from an overseas supplier
      *>   under the reverse charge.  The supplier charges no VAT, so
      *>   the payable is the net 500.00; we self account 100.00 in
      *>   both the output and the input box.
       2000-REVERSE-CHARGE-LINE.
           INITIALIZE WS-APLIN
           MOVE "BOTH" TO LN-FUNC
           MOVE "STC" TO LN-COMPANY
           MOVE 20261015 TO LN-DATE
           MOVE 1 TO LN-LINE-NUM
           MOVE "Design services, overseas supplier"
               TO LN-DESCRIPTION
           MOVE "ZZAPH-SVC" TO LN-SERVICE
           MOVE "6800" TO LN-ACCOUNT
           MOVE "ADMIN" TO LN-COST-CTR
           MOVE 1 TO LN-QTY
           MOVE 500.00 TO LN-PRICE
           MOVE 1 TO LN-PRICE-PER
           MOVE 1 TO LN-FX-RATE
           MOVE "RC" TO LN-VAT-CODE
           MOVE 20261015 TO LN-TAX-POINT
           CALL "APINVLIN" USING WS-APLIN
           MOVE "a reverse charge line codes cleanly" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE LN-RC TO TK-ACTUAL-N
           MOVE LN-MESSAGE TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the net is 500.00" TO TK-NAME
           MOVE 500.00 TO TK-EXPECTED-N
           MOVE LN-NET-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the supplier charges no VAT at all" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE LN-VAT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the invoice is therefore paid net" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE LN-SELF-ACCOUNTS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "100.00 is self accounted at 20 per cent" TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE LN-SELF-VAT-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "and the same 100.00 is recoverable input tax"
               TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE LN-RECOVER-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "none of it is charged to the cost" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE LN-NONRECOVER-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the input tax belongs in box 4" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE LN-VAT-BOX TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "and the net value in box 7" TO TK-NAME
           MOVE 7 TO TK-EXPECTED-N
           MOVE LN-NET-BOX TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AP-021 - recoverable and non recoverable input tax are
      *>   accounted separately; without valid evidence the tax falls
      *>   to the cost instead of the VAT account
       2100-NON-RECOVERABLE-VAT.
           INITIALIZE WS-APLIN
           MOVE "BOTH" TO LN-FUNC
           MOVE "STC" TO LN-COMPANY
           MOVE 20261015 TO LN-DATE
           MOVE "Abrasive discs" TO LN-DESCRIPTION
           MOVE "ZZAPH-PO" TO LN-ORDER
           MOVE "6800" TO LN-ACCOUNT
           MOVE 1 TO LN-QTY
           MOVE 500.00 TO LN-PRICE
           MOVE 1 TO LN-PRICE-PER
           MOVE 1 TO LN-FX-RATE
           MOVE "S" TO LN-VAT-CODE
           MOVE 20261015 TO LN-TAX-POINT
           MOVE "Y" TO LN-VAT-EVIDENCE
           CALL "APINVLIN" USING WS-APLIN
           MOVE "standard rated input tax is 100.00" TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE LN-VAT TO TK-ACTUAL-N
           MOVE LN-MESSAGE TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "with evidence it is all recoverable" TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE LN-RECOVER-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the supplier does charge this VAT" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE LN-SELF-ACCOUNTS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "N" TO LN-VAT-EVIDENCE
           MOVE 0 TO LN-NET
           CALL "APINVLIN" USING WS-APLIN
           MOVE "without evidence none of it is recoverable"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE LN-RECOVER-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the whole 100.00 is accounted as a cost"
               TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE LN-NONRECOVER-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AP-021 / AC-038 - the same 100.00 goes into the output box
      *>   and the input box, so the reverse charge costs nothing in
      *>   tax and the supplier is paid the net amount
       2200-REVERSE-CHARGE-BOXES.
           PERFORM 0600-VAT-HEADER
           MOVE "RCHARGE" TO CA-FUNCTION OF WS-VAT-CA
           MOVE "RC" TO VE-VAT-CODE
           MOVE 500.00 TO VE-NET-GBP
           MOVE 20.0000 TO VE-RATE-PCT
           MOVE "Y" TO VE-IS-RECOVERABLE
           CALL "APVATEVD" USING WS-VAT-CA
           MOVE "100.00 of output tax is self accounted" TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE VE-OUTPUT-VAT-GBP TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-VAT-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "and 100.00 is reclaimed as input tax" TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE VE-INPUT-VAT-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "so the net effect on the return is nil" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           COMPUTE WS-NUM = VE-OUTPUT-VAT-GBP - VE-INPUT-VAT-GBP
           MOVE WS-NUM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the output goes to box 1" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE VE-VAT-BOX TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the supply is declared in box 7" TO TK-NAME
           MOVE 7 TO TK-EXPECTED-N
           MOVE VE-NET-BOX TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the company accounts for the tax itself" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE VE-SELF-ACCOUNTS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0600-VAT-HEADER
           MOVE "RCHARGE" TO CA-FUNCTION OF WS-VAT-CA
           MOVE "RC" TO VE-VAT-CODE
           MOVE 500.00 TO VE-NET-GBP
           MOVE 20.0000 TO VE-RATE-PCT
           MOVE "N" TO VE-IS-RECOVERABLE
           CALL "APVATEVD" USING WS-VAT-CA
           MOVE "where the supply is not recoverable, none is"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE VE-INPUT-VAT-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the 100.00 is accounted separately as a cost"
               TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE VE-NONRECOVER-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0600-VAT-HEADER
           MOVE "SPLIT" TO CA-FUNCTION OF WS-VAT-CA
           MOVE 100.00 TO VE-CLAIMED-VAT-GBP
           MOVE 60.0000 TO VE-RECOVER-PCT
           MOVE "Y" TO VE-IS-RECOVERABLE
           CALL "APVATEVD" USING WS-VAT-CA
           MOVE "a 60 per cent business use recovers 60.00"
               TO TK-NAME
           MOVE 60.00 TO TK-EXPECTED-N
           MOVE VE-ALLOWED-VAT-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the other 40.00 is non recoverable" TO TK-NAME
           MOVE 40.00 TO TK-EXPECTED-N
           MOVE VE-NONRECOVER-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AP-021 - import VAT follows the approved policy: it is only
      *>   released against the C79 or postponed import VAT statement,
      *>   and the payable itself is unaffected
       2300-IMPORT-VAT-EVIDENCE.
           PERFORM 0600-VAT-HEADER
           MOVE "EVIDENCE" TO CA-FUNCTION OF WS-VAT-CA
           MOVE "PVASTATEMENT" TO VE-EVIDENCE-TYPE
           MOVE SPACES TO VE-EVIDENCE-REF
           MOVE 100.00 TO VE-CLAIMED-VAT-GBP
           CALL "APVATEVD" USING WS-VAT-CA
           MOVE "import VAT without its statement is held"
               TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-VAT-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-VAT-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the evidence rule is quoted" TO TK-NAME
           MOVE "APV0010" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-VAT-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "none of the tax is released" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE VE-ALLOWED-VAT-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the whole 100.00 is held for review" TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE VE-HELD-VAT-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0600-VAT-HEADER
           MOVE "EVIDENCE" TO CA-FUNCTION OF WS-VAT-CA
           MOVE "PVASTATEMENT" TO VE-EVIDENCE-TYPE
           MOVE "ZZAPH-PVA-2026-10" TO VE-EVIDENCE-REF
           MOVE 100.00 TO VE-CLAIMED-VAT-GBP
           CALL "APVATEVD" USING WS-VAT-CA
           MOVE "with the statement the import VAT is released"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-VAT-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-VAT-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "and all 100.00 of it may be claimed" TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE VE-ALLOWED-VAT-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AP-027 / AC-127 - the supplier emails new bank details.  The
      *>   destination is recorded but is not payable, and it cannot be
      *>   approved on the strength of the email alone.
       2400-EMAIL-ALONE-CANNOT-PAY.
           PERFORM 0700-BANK-HEADER
           MOVE "SYSADM" TO CA-USER OF WS-BNK-CA
           MOVE "ZZAPH-BANK-PREP" TO CA-CORRELATION-ID OF WS-BNK-CA
           MOVE "PREPARE" TO CA-FUNCTION OF WS-BNK-CA
           MOVE "Gap Test Precision Ltd" TO BV-BENEFICIARY
           MOVE "GB" TO BV-COUNTRY
           MOVE "GBP" TO BV-CURRENCY
           MOVE "309876" TO BV-SORT-CODE
           MOVE "87654321" TO BV-ACCOUNT-NO
           MOVE "BACS" TO BV-PAY-METHOD
           MOVE "EMAIL" TO BV-REQUEST-SOURCE
           MOVE 20261015 TO BV-VALID-FROM
           CALL "APBANKVR" USING WS-BNK-CA
           MOVE "the requested destination is recorded" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-BNK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-BNK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "it is recorded as pending, not usable" TO TK-NAME
           MOVE "PENDING" TO TK-EXPECTED-X
           MOVE BV-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "it cannot be paid to yet" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE BV-USABLE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "it is a change, so it supersedes a destination"
               TO TK-NAME
           IF BV-SUPERSEDES-ID > 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           PERFORM 0700-BANK-HEADER
           MOVE "TREASURY" TO CA-USER OF WS-BNK-CA
           MOVE "CHECK" TO CA-FUNCTION OF WS-BNK-CA
           CALL "APBANKVR" USING WS-BNK-CA
           MOVE "the payment run will not use it" TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-BNK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-BNK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the unverified destination is named" TO TK-NAME
           MOVE "APB0021" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-BNK-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0700-BANK-HEADER
           MOVE "TREASURY" TO CA-USER OF WS-BNK-CA
           MOVE "ZZAPH-BANK-APP0" TO CA-CORRELATION-ID OF WS-BNK-CA
           MOVE "APPROVE" TO CA-FUNCTION OF WS-BNK-CA
           CALL "APBANKVR" USING WS-BNK-CA
           MOVE "an emailed change cannot be approved as it stands"
               TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-BNK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-BNK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the missing verification is named" TO TK-NAME
           MOVE "APB0016" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-BNK-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AP-027 / AC-127 - the Confirmation of Payee comes back as a
      *>   close match, which needs an authorised decision; and the
      *>   call-back cannot run before the check it relies on
       2500-COP-NEEDS-A-DECISION.
           PERFORM 0700-BANK-HEADER
           MOVE "SYSADM" TO CA-USER OF WS-BNK-CA
           MOVE "CALLBACK" TO CA-FUNCTION OF WS-BNK-CA
           MOVE "Accounts contact 0118 496 0000 held since 2019"
               TO BV-CALLBACK-SOURCE
           MOVE "Y" TO BV-CALLBACK-ON-FILE
           CALL "APBANKVR" USING WS-BNK-CA
           MOVE "the call-back cannot precede the payee check"
               TO TK-NAME
           MOVE "APB0015" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-BNK-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-BNK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           PERFORM 0700-BANK-HEADER
           MOVE "SYSADM" TO CA-USER OF WS-BNK-CA
           MOVE "COP" TO CA-FUNCTION OF WS-BNK-CA
           MOVE "CLOSEMATCH" TO BV-COP-RESULT
           MOVE "ZZAPH-COP-001" TO BV-COP-REFERENCE
           MOVE SPACES TO BV-COP-DECISION
           CALL "APBANKVR" USING WS-BNK-CA
           MOVE "a close match on its own is held" TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-BNK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-BNK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the missing decision is named" TO TK-NAME
           MOVE "APB0012" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-BNK-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0700-BANK-HEADER
           MOVE "SYSADM" TO CA-USER OF WS-BNK-CA
           MOVE "COP" TO CA-FUNCTION OF WS-BNK-CA
           MOVE "CLOSEMATCH" TO BV-COP-RESULT
           MOVE "ZZAPH-COP-001" TO BV-COP-REFERENCE
           MOVE "Trading name differs from the registered name"
               TO BV-COP-DECISION
           CALL "APBANKVR" USING WS-BNK-CA
           MOVE "a recorded decision accepts the close match"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-BNK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-BNK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the payee check is now on the record" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE BV-COP-DONE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AP-027 / AC-127 - a call-back to the number that came with
      *>   the change request is exactly the fraud this control stops,
      *>   so it is refused outright
       2600-CALLBACK-MUST-BE-ON-FILE.
           PERFORM 0700-BANK-HEADER
           MOVE "SYSADM" TO CA-USER OF WS-BNK-CA
           MOVE "CALLBACK" TO CA-FUNCTION OF WS-BNK-CA
           MOVE "Number given in the email asking for the change"
               TO BV-CALLBACK-SOURCE
           MOVE "N" TO BV-CALLBACK-ON-FILE
           CALL "APBANKVR" USING WS-BNK-CA
           MOVE "calling the number in the email verifies nothing"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-BNK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-BNK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the contact on record rule is quoted" TO TK-NAME
           MOVE "APB0014" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-BNK-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0700-BANK-HEADER
           MOVE "SYSADM" TO CA-USER OF WS-BNK-CA
           MOVE "CALLBACK" TO CA-FUNCTION OF WS-BNK-CA
           MOVE SPACES TO BV-CALLBACK-SOURCE
           MOVE "Y" TO BV-CALLBACK-ON-FILE
           CALL "APBANKVR" USING WS-BNK-CA
           MOVE "the call-back must say who was telephoned"
               TO TK-NAME
           MOVE "APB0013" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-BNK-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-BNK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           PERFORM 0700-BANK-HEADER
           MOVE "SYSADM" TO CA-USER OF WS-BNK-CA
           MOVE "CALLBACK" TO CA-FUNCTION OF WS-BNK-CA
           MOVE "Accounts contact 0118 496 0000 held since 2019"
               TO BV-CALLBACK-SOURCE
           MOVE "Y" TO BV-CALLBACK-ON-FILE
           MOVE "Spoke to the finance manager we already deal with"
               TO BV-CALLBACK-NOTE
           CALL "APBANKVR" USING WS-BNK-CA
           MOVE "a call-back to the contact on record verifies it"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-BNK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-BNK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the destination is now verified" TO TK-NAME
           MOVE "VERIFIED" TO TK-EXPECTED-X
           MOVE BV-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "it still may not be paid to" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE BV-USABLE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AP-027 / AC-127 - the preparer may not approve the change,
      *>   and the approval keeps the payee result with it
       2700-APPROVAL-IS-INDEPENDENT.
           PERFORM 0700-BANK-HEADER
           MOVE "SYSADM" TO CA-USER OF WS-BNK-CA
           MOVE "ZZAPH-BANK-APP1" TO CA-CORRELATION-ID OF WS-BNK-CA
           MOVE "APPROVE" TO CA-FUNCTION OF WS-BNK-CA
           CALL "APBANKVR" USING WS-BNK-CA
           MOVE "the preparer cannot approve the change" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-BNK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-BNK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "self approval is named" TO TK-NAME
           MOVE "APB0017" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-BNK-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0700-BANK-HEADER
           MOVE "TREASURY" TO CA-USER OF WS-BNK-CA
           MOVE "ZZAPH-BANK-APP2" TO CA-CORRELATION-ID OF WS-BNK-CA
           MOVE "APPROVE" TO CA-FUNCTION OF WS-BNK-CA
           CALL "APBANKVR" USING WS-BNK-CA
           MOVE "an independent approver may approve it" TO TK-NAME
           MOVE CA-MESSAGE OF WS-BNK-CA TO TK-DETAIL
           IF CA-RETURN-CODE OF WS-BNK-CA < 6
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           MOVE "the destination becomes approved" TO TK-NAME
           MOVE "APPROVED" TO TK-EXPECTED-X
           MOVE BV-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "and only now may it be paid to" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE BV-USABLE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0700-BANK-HEADER
           MOVE "TREASURY" TO CA-USER OF WS-BNK-CA
           MOVE "CHECK" TO CA-FUNCTION OF WS-BNK-CA
           CALL "APBANKVR" USING WS-BNK-CA
           MOVE "the payment run now accepts the destination"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-BNK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-BNK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the close match result is retained with it"
               TO TK-NAME
           MOVE "CLOSEMATCH" TO TK-EXPECTED-X
           MOVE BV-COP-RESULT TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "so is the decision that accepted it" TO TK-NAME
           IF BV-COP-DECISION NOT = SPACES
               MOVE BV-COP-DECISION TO TK-DETAIL
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Status, ISNULL(ChangeRequestSource,'') "
                  "FROM ap.SupplierBank WHERE CompanyCode = 'STC' "
                  "AND SupplierCode = 'ZZAPH1' AND BankRef = 'MAIN'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8100-READ-TWO
           MOVE "the destination it replaced is superseded"
               TO TK-NAME
           MOVE "SUPERSEDED" TO TK-EXPECTED-X
           MOVE WS-R2-ONE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(ChangeRequestSource,''), Status "
                  "FROM ap.SupplierBank WHERE CompanyCode = 'STC' "
                  "AND SupplierCode = 'ZZAPH1' AND BankRef = 'NEWBK'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8100-READ-TWO
           MOVE "the email that asked for it is kept on record"
               TO TK-NAME
           MOVE "EMAIL" TO TK-EXPECTED-X
           MOVE WS-R2-ONE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AP-030 - a statutory claim is interest at base plus the
      *>   margin for the days the debt ran late, plus a fixed sum of
      *>   compensation by debt band.  It is coded to the finance cost
      *>   account, never to the cost of the goods it was late for.
       2800-LATE-PAYMENT-IS-FINANCE-COST.
           PERFORM 0800-LATE-HEADER
           MOVE 5.2500 TO LP-BASE-RATE-PCT
           MOVE 5000.00 TO LP-DEBT-GBP
           MOVE 20260630 TO LP-DUE-DATE
           MOVE 20260829 TO LP-PAID-DATE
           MOVE "CALC" TO CA-FUNCTION OF WS-LTE-CA
           CALL "APLATEPY" USING WS-LTE-CA
           MOVE "the debt was paid sixty days after it fell due"
               TO TK-NAME
           MOVE CA-MESSAGE OF WS-LTE-CA TO TK-DETAIL
           MOVE 60 TO TK-EXPECTED-N
           MOVE LP-DAYS-LATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the statutory rate is base plus the eight per cent"
               TO TK-NAME
           MOVE 13.2500 TO TK-EXPECTED-N
           MOVE LP-STATUTORY-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "interest of 108.90 accrues over those sixty days"
               TO TK-NAME
           MOVE 108.90 TO TK-EXPECTED-N
           MOVE LP-INTEREST-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "a debt of 5,000.00 carries the 70.00 fixed sum"
               TO TK-NAME
           MOVE 70.00 TO TK-EXPECTED-N
           MOVE LP-COMPENSATION-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the claim is the interest plus that fixed sum"
               TO TK-NAME
           MOVE 178.90 TO TK-EXPECTED-N
           MOVE LP-CLAIMED-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "it is coded to finance cost, not to the goods"
               TO TK-NAME
           MOVE "7200" TO TK-EXPECTED-X
           MOVE LP-FINANCE-ACCOUNT TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the statute it is claimed under is named" TO TK-NAME
           MOVE "LPCDIA1998" TO TK-EXPECTED-X
           MOVE LP-STATUTE-REF TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
      *>   the fixed sum is banded, not accrued: under 1,000 it is
      *>   40.00 and from 10,000 it is 100.00
           PERFORM 0800-LATE-HEADER
           MOVE 5.2500 TO LP-BASE-RATE-PCT
           MOVE 999.99 TO LP-DEBT-GBP
           MOVE 20260630 TO LP-DUE-DATE
           MOVE 20260829 TO LP-PAID-DATE
           MOVE "CALC" TO CA-FUNCTION OF WS-LTE-CA
           CALL "APLATEPY" USING WS-LTE-CA
           MOVE "a debt just under 1,000.00 carries 40.00"
               TO TK-NAME
           MOVE 40.00 TO TK-EXPECTED-N
           MOVE LP-COMPENSATION-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0800-LATE-HEADER
           MOVE 5.2500 TO LP-BASE-RATE-PCT
           MOVE 10000.00 TO LP-DEBT-GBP
           MOVE 20260630 TO LP-DUE-DATE
           MOVE 20260829 TO LP-PAID-DATE
           MOVE "CALC" TO CA-FUNCTION OF WS-LTE-CA
           CALL "APLATEPY" USING WS-LTE-CA
           MOVE "a debt of 10,000.00 carries 100.00" TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE LP-COMPENSATION-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
      *>   a debt settled on time gives rise to no claim at all
           PERFORM 0800-LATE-HEADER
           MOVE 5.2500 TO LP-BASE-RATE-PCT
           MOVE 5000.00 TO LP-DEBT-GBP
           MOVE 20260630 TO LP-DUE-DATE
           MOVE 20260615 TO LP-PAID-DATE
           MOVE "CALC" TO CA-FUNCTION OF WS-LTE-CA
           CALL "APLATEPY" USING WS-LTE-CA
           MOVE "a debt paid before it fell due is not late"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE LP-DAYS-LATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "and no statutory claim arises from it" TO TK-NAME
           MOVE CA-MESSAGE OF WS-LTE-CA TO TK-DETAIL
           MOVE 0 TO TK-EXPECTED-N
           MOVE LP-CLAIMED-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AP-030 - agreed terms are checked against the statutory and
      *>   policy maximum, and terms beyond the maximum for a small
      *>   supplier need an authorised exception rather than a warning
       2900-TERMS-AGAINST-THE-LIMIT.
           PERFORM 0800-LATE-HEADER
           MOVE "SMALL" TO LP-BUSINESS-SIZE
           MOVE 90 TO LP-TERM-DAYS
           MOVE "TERMCHK" TO CA-FUNCTION OF WS-LTE-CA
           CALL "APLATEPY" USING WS-LTE-CA
           MOVE "ninety day terms for a small supplier are refused"
               TO TK-NAME
           MOVE CA-MESSAGE OF WS-LTE-CA TO TK-DETAIL
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-LTE-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "they need an authorised exception" TO TK-NAME
           MOVE "APT0012" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-LTE-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "and the exception flag says so" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE LP-TERM-EXCEPTION TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0800-LATE-HEADER
           MOVE "LARGE" TO LP-BUSINESS-SIZE
           MOVE 90 TO LP-TERM-DAYS
           MOVE "TERMCHK" TO CA-FUNCTION OF WS-LTE-CA
           CALL "APLATEPY" USING WS-LTE-CA
           MOVE "the same terms for a large supplier only warn"
               TO TK-NAME
           MOVE CA-MESSAGE OF WS-LTE-CA TO TK-DETAIL
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-LTE-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "under its own reason code" TO TK-NAME
           MOVE "APT0013" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-LTE-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0800-LATE-HEADER
           MOVE "SMALL" TO LP-BUSINESS-SIZE
           MOVE 60 TO LP-TERM-DAYS
           MOVE "TERMCHK" TO CA-FUNCTION OF WS-LTE-CA
           CALL "APLATEPY" USING WS-LTE-CA
           MOVE "sixty days is the maximum, so it is allowed"
               TO TK-NAME
           MOVE CA-MESSAGE OF WS-LTE-CA TO TK-DETAIL
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-LTE-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "and needs no exception" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE LP-TERM-EXCEPTION TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
      *>   the recorded terms code decides the days, not a typed number
           PERFORM 0800-LATE-HEADER
           MOVE "SMALL" TO LP-BUSINESS-SIZE
           MOVE "N30" TO LP-TERMS-CODE
           MOVE "TERMCHK" TO CA-FUNCTION OF WS-LTE-CA
           CALL "APLATEPY" USING WS-LTE-CA
           MOVE "the agreed N30 terms are read as thirty days"
               TO TK-NAME
           MOVE CA-MESSAGE OF WS-LTE-CA TO TK-DETAIL
           MOVE 30 TO TK-EXPECTED-N
           MOVE LP-TERM-DAYS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "thirty days is within the approved maximum"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-LTE-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       COPY TESTKITP.
