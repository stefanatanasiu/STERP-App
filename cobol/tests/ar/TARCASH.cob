      *>================================================================
      *> PROGRAM-ID : TARCASH
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR) UNIT TESTS
      *> PURPOSE    : RCPTALOC AND ARADJUST - RECEIPTS, ALLOCATION,
      *>              CORRECTION HISTORY, SETTLEMENT DISCOUNT, THE
      *>              D-22 GBP 5.00 SMALL DIFFERENCE, WRITE OFFS AND
      *>              FOREIGN CURRENCY RECEIPTS WITH REALISED FX.
      *>              WORKED NUMBERS FROM THE DEBTORS SPECIFICATION
      *>              SECTION 7 AND SCENARIOS AC-022 / AC-023 / AC-026:
      *>                1,200.00 invoice, 700.00 receipt -> 500.00,
      *>                a further 480.00 -> 20.00 still outstanding.
      *>              BOTH PROGRAMS OWN THEIR TRANSACTIONS, SO THE
      *>              SUITE USES UNIQUE ZZCASH KEYS AND CLEANS UP.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: AR-009, AR-010, AR-011, AR-012, AR-019, AR-020, D-22
      *> AC: AC-022, AC-023, AC-026
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TARCASH.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY RCPTALOCCA REPLACING ==DFHCOMMAREA== BY ==WS-RA-CA==.
       COPY ARADJUSTCA REPLACING ==DFHCOMMAREA== BY ==WS-AJ-CA==.
       COPY DISHONORCA REPLACING ==DFHCOMMAREA== BY ==WS-DH-CA==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "Y".
       01  WS-PROBE.
           05  WS-PROBE-COL            PIC X(30).
           05  WS-PROBE-2              PIC X(30).
       01  WS-KEEP.
           05  WS-R1-ITEM              PIC S9(9) COMP-5 VALUE 0.
           05  WS-R2-ITEM              PIC S9(9) COMP-5 VALUE 0.
           05  WS-R3-ITEM              PIC S9(9) COMP-5 VALUE 0.
           05  WS-R4-ITEM              PIC S9(9) COMP-5 VALUE 0.
           05  WS-R5-ITEM              PIC S9(9) COMP-5 VALUE 0.
           05  WS-R1-NO                PIC X(30) VALUE SPACES.
           05  WS-ALLOC-1              PIC S9(9) COMP-5 VALUE 0.
           05  WS-DH-ITEM              PIC S9(9) COMP-5 VALUE 0.
           05  WS-DH-NO                PIC X(30) VALUE SPACES.
       01  WS-I                        PIC S9(4) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TARCASH receipts and allocation" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-EVERYTHING
           ELSE
               PERFORM 0150-CLEAN-UP
               PERFORM 0160-BUILD-FIXTURE
               PERFORM 1000-RECORD-RECEIPTS
               PERFORM 1100-PREVIEW-HAS-NO-EFFECT
               PERFORM 1200-PARTIAL-ALLOCATION
               PERFORM 1300-WRITE-OFF-LIMITS
               PERFORM 1400-FOREIGN-CURRENCY
               PERFORM 1500-UNALLOCATION
               PERFORM 1600-ADJUSTMENT-AUTHORITY
               PERFORM 1700-DISHONOUR-REINSTATES
               PERFORM 0150-CLEAN-UP
           END-IF
           PERFORM TK-END
           STOP RUN.

       0100-PROBE-DATABASE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CustomerCode, '' FROM ar.Customer "
                  "WHERE CompanyCode = 'STC'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "N" TO WS-DB-UP
           END-IF.

       0170-RUN-SQL.
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR
               DISPLAY "# fixture SQL: "
                       FUNCTION TRIM(SQLIO-ERRMSG) UPON SYSOUT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

       0180-READ-ONE.
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 7 TO SQLIO-CURSOR
           MOVE SPACES TO WS-PROBE
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

       0150-CLEAN-UP.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; "
                  "DELETE ar.Allocation WHERE CompanyCode = 'STC' "
                  "AND CustomerCode LIKE 'ZZCASH%'; "
                  "DELETE ar.OpenItem WHERE CompanyCode = 'STC' "
                  "AND CustomerCode LIKE 'ZZCASH%'; "
                  "DELETE ar.Receipt WHERE CompanyCode = 'STC' "
                  "AND (CustomerCode LIKE 'ZZCASH%' OR "
                  "CorrelationId LIKE 'ZZARCASH%'); "
                  "DELETE ar.Customer WHERE CompanyCode = 'STC' "
                  "AND CustomerCode LIKE 'ZZCASH%'; "
                  "DELETE fin.JournalLine WHERE JournalId IN "
                  "(SELECT JournalId FROM fin.Journal WHERE "
                  "CorrelationId LIKE 'ZZARCASH%'); "
                  "DELETE fin.Journal WHERE CorrelationId LIKE "
                  "'ZZARCASH%'; "
                  "DELETE fin.JournalStage WHERE CorrelationId LIKE "
                  "'ZZARCASH%'; "
                  "DELETE core.IdempotencyKey WHERE CorrelationId "
                  "LIKE 'ZZARCASH%'; "
                  "DELETE core.ExceptionItem WHERE CorrelationId "
                  "LIKE 'ZZARCASH%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0170-RUN-SQL.

       0160-BUILD-FIXTURE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT ar.Customer (CompanyCode, "
                  "CustomerCode, LegalName, LegalForm, VatLocation, "
                  "CountryCode, CurrencyCode, AllowedCurrencies, "
                  "Status, SanctionsStatus, CreatedBy) VALUES "
                  "('STC','ZZCASH1','Zed Cash Test Limited','LTD',"
                  "'GB','GB','GBP','GBP,USD','ACTIVE','CLEAR',"
                  "'ZZARTEST'), "
                  "('STC','ZZCASH2','Zed Other Debtor Limited','LTD',"
                  "'GB','GB','GBP','GBP','ACTIVE','CLEAR','ZZARTEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0170-RUN-SQL
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT ar.OpenItem (CompanyCode, "
                  "CustomerCode, ItemType, DocumentNumber, "
                  "DocumentDate, DueDate, PostingDate, CurrencyCode, "
                  "FxRate, OriginalAmount, OriginalAmountGBP, "
                  "ResidualAmount, ResidualAmountGBP, VatAmountGBP, "
                  "DiscountDueDate, DiscountAmount, "
                  "SettlementStatus, CreatedBy) VALUES "
                  "('STC','ZZCASH1','INVOICE','ZZCS-INV-1',"
                  "'2026-10-01','2026-11-15','2026-10-01','GBP',1,"
                  "1200.00,1200.00,1200.00,1200.00,200.00,NULL,NULL,"
                  "'OPEN','ZZARTEST'), "
                  "('STC','ZZCASH1','INVOICE','ZZCS-INV-2',"
                  "'2026-10-01','2026-11-15','2026-10-01','GBP',1,"
                  "500.00,500.00,500.00,500.00,0,'2026-10-20',10.00,"
                  "'OPEN','ZZARTEST'), "
                  "('STC','ZZCASH1','INVOICE','ZZCS-INV-3',"
                  "'2026-10-01','2026-11-15','2026-10-01','USD',"
                  "1.25,1200.00,960.00,1200.00,960.00,0,NULL,NULL,"
                  "'OPEN','ZZARTEST'), "
                  "('STC','ZZCASH1','INVOICE','ZZCS-INV-4',"
                  "'2026-10-01','2026-11-15','2026-10-01','GBP',1,"
                  "100.00,100.00,100.00,100.00,0,NULL,NULL,'OPEN',"
                  "'ZZARTEST'), "
                  "('STC','ZZCASH2','INVOICE','ZZCS-INV-9',"
                  "'2026-10-01','2026-11-15','2026-10-01','GBP',1,"
                  "200.00,200.00,200.00,200.00,0,NULL,NULL,'OPEN',"
                  "'ZZARTEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0170-RUN-SQL.

       0250-BUILD-RECEIPT.
           INITIALIZE WS-RA-CA
           MOVE "STC" TO CA-COMPANY OF WS-RA-CA
           MOVE "RDG" TO CA-SITE OF WS-RA-CA
           MOVE "ARCLERK" TO CA-USER OF WS-RA-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-RA-CA
           MOVE "RECEIPT" TO CA-FUNCTION OF WS-RA-CA
           MOVE "ZZCASH1" TO RA-CUSTOMER
           MOVE "BACS" TO RA-METHOD
           MOVE "GBP1" TO RA-BANK-ACCOUNT
           MOVE 20261015 TO RA-RECEIPT-DATE
           MOVE 20261015 TO RA-POSTING-DATE
           MOVE "GBP" TO RA-CURRENCY
           MOVE "CONFIRMED" TO RA-CONFIRM-STATE.

       0260-BUILD-ALLOC.
           INITIALIZE WS-RA-CA
           MOVE "STC" TO CA-COMPANY OF WS-RA-CA
           MOVE "RDG" TO CA-SITE OF WS-RA-CA
           MOVE "ARCLERK" TO CA-USER OF WS-RA-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-RA-CA
           MOVE "ALLOCATE" TO CA-FUNCTION OF WS-RA-CA
           MOVE 20261015 TO RA-POSTING-DATE
           MOVE 20261015 TO RA-EFFECTIVE-DATE
           MOVE 1 TO RA-ALLOC-CNT.

      *>   AR-009 - confirmed funds, and a repeated bank receipt that
      *>   does not increase cash twice
       1000-RECORD-RECEIPTS.
           PERFORM 0250-BUILD-RECEIPT
           MOVE 700.00 TO RA-GROSS
           MOVE "ZZARCASH-R1" TO CA-CORRELATION-ID OF WS-RA-CA
           CALL "RCPTALOC" USING WS-RA-CA
           IF CA-RETURN-CODE OF WS-RA-CA NOT = 0
               DISPLAY "# receipt: "
                       FUNCTION TRIM(CA-REASON-CODE OF WS-RA-CA) " "
                       FUNCTION TRIM(CA-MESSAGE OF WS-RA-CA)
                   UPON SYSOUT
           END-IF
           MOVE "a confirmed 700.00 receipt is recorded" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-RA-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "it is valued at 700.00 in sterling" TO TK-NAME
           MOVE 700.00 TO TK-EXPECTED-N
           MOVE RA-GROSS-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "it draws its own receipt number" TO TK-NAME
           IF RA-RECEIPT-NO NOT = SPACES AND RA-SOURCE-ITEM-ID > 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           MOVE RA-SOURCE-ITEM-ID TO WS-R1-ITEM
           MOVE RA-RECEIPT-NO TO WS-R1-NO
           PERFORM 0250-BUILD-RECEIPT
           MOVE 700.00 TO RA-GROSS
           MOVE "ZZARCASH-R1" TO CA-CORRELATION-ID OF WS-RA-CA
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE "a repeated bank receipt warns, it does not re-bank"
               TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-RA-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "it answers with the original receipt" TO TK-NAME
           MOVE WS-R1-NO TO TK-EXPECTED-X
           MOVE RA-RECEIPT-NO TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 1050-COUNT-RECEIPTS
           MOVE "cash was banked once, not twice" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-PROBE-COL)
           PERFORM TK-ASSERT-NUM
           PERFORM 0250-BUILD-RECEIPT
           MOVE 480.00 TO RA-GROSS
           MOVE "ZZARCASH-R2" TO CA-CORRELATION-ID OF WS-RA-CA
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE RA-SOURCE-ITEM-ID TO WS-R2-ITEM
           MOVE "a second receipt of 480.00 is recorded" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-RA-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           PERFORM 0250-BUILD-RECEIPT
           MOVE 50.00 TO RA-GROSS
           MOVE "EXPECTED" TO RA-CONFIRM-STATE
           MOVE "ZZARCASH-R5" TO CA-CORRELATION-ID OF WS-RA-CA
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE "unconfirmed evidence is still recorded" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-RA-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           PERFORM 0250-BUILD-RECEIPT
           MOVE 0 TO RA-GROSS
           MOVE "ZZARCASH-R6" TO CA-CORRELATION-ID OF WS-RA-CA
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE "a receipt of nil is refused" TO TK-NAME
           MOVE "RCP0002" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RA-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0250-BUILD-RECEIPT
           MOVE 100.00 TO RA-GROSS
           MOVE SPACES TO CA-CORRELATION-ID OF WS-RA-CA
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE "a receipt without a correlation id is refused"
               TO TK-NAME
           MOVE "RCP0007" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RA-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       1050-COUNT-RECEIPTS.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), COUNT(*)), "
                  "CONVERT(varchar(24), ISNULL(SUM("
                  "GrossReceivedGBP),0)) FROM ar.Receipt WHERE "
                  "CompanyCode = 'STC' AND CorrelationId = "
                  "'ZZARCASH-R1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0180-READ-ONE.

       1060-READ-ITEM-1.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), ResidualAmountGBP), "
                  "SettlementStatus FROM ar.OpenItem WHERE "
                  "CompanyCode = 'STC' AND DocumentNumber = "
                  "'ZZCS-INV-1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0180-READ-ONE.

      *>   AR-010 - a preview shows the residual without any effect
       1100-PREVIEW-HAS-NO-EFFECT.
           PERFORM 0260-BUILD-ALLOC
           MOVE "PREVIEW" TO CA-FUNCTION OF WS-RA-CA
           MOVE WS-R1-ITEM TO RA-SOURCE-ITEM-ID
           MOVE "ZZCS-INV-1" TO RA-A-DOC-NO(1)
           MOVE 700.00 TO RA-A-APPLY(1)
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE "the preview is accepted" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-RA-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the invoice stands at 1,200.00 before" TO TK-NAME
           MOVE 1200.00 TO TK-EXPECTED-N
           MOVE RA-A-RESID-BEFORE(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "700.00 would leave 500.00" TO TK-NAME
           MOVE 500.00 TO TK-EXPECTED-N
           MOVE RA-A-RESID-AFTER(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 1060-READ-ITEM-1
           MOVE "a preview has no effect on the ledger" TO TK-NAME
           MOVE 1200.00 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-PROBE-COL)
           PERFORM TK-ASSERT-NUM
           PERFORM 0260-BUILD-ALLOC
           MOVE "PREVIEW" TO CA-FUNCTION OF WS-RA-CA
           MOVE WS-R1-ITEM TO RA-SOURCE-ITEM-ID
           MOVE "ZZCS-INV-1" TO RA-A-DOC-NO(1)
           MOVE 1300.00 TO RA-A-APPLY(1)
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE "more than the receipt holds cannot be applied"
               TO TK-NAME
           MOVE "RCP0031" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RA-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           PERFORM 0260-BUILD-ALLOC
           MOVE "PREVIEW" TO CA-FUNCTION OF WS-RA-CA
           MOVE WS-R1-ITEM TO RA-SOURCE-ITEM-ID
           MOVE "ZZCS-INV-2" TO RA-A-DOC-NO(1)
           MOVE 600.00 TO RA-A-APPLY(1)
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE "more than the invoice owes cannot be applied"
               TO TK-NAME
           MOVE "RCP0032" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RA-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           PERFORM 0260-BUILD-ALLOC
           MOVE "PREVIEW" TO CA-FUNCTION OF WS-RA-CA
           MOVE WS-R1-ITEM TO RA-SOURCE-ITEM-ID
           MOVE "ZZCS-INV-9" TO RA-A-DOC-NO(1)
           MOVE 100.00 TO RA-A-APPLY(1)
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE "money of one debtor cannot settle another's debt"
               TO TK-NAME
           MOVE "RCP0024" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RA-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

      *>   AC-022 / AC-023 - 1,200.00 less 700.00 less 480.00 leaves
      *>   20.00 which is not silently settled
       1200-PARTIAL-ALLOCATION.
           PERFORM 0260-BUILD-ALLOC
           MOVE WS-R1-ITEM TO RA-SOURCE-ITEM-ID
           MOVE "ZZCS-INV-1" TO RA-A-DOC-NO(1)
           MOVE 700.00 TO RA-A-APPLY(1)
           CALL "RCPTALOC" USING WS-RA-CA
           IF CA-RETURN-CODE OF WS-RA-CA NOT = 0
               DISPLAY "# alloc: "
                       FUNCTION TRIM(CA-REASON-CODE OF WS-RA-CA) " "
                       FUNCTION TRIM(CA-MESSAGE OF WS-RA-CA)
                   UPON SYSOUT
           END-IF
           MOVE "the 700.00 receipt is allocated" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-RA-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE RA-A-ALLOC-ID(1) TO WS-ALLOC-1
           MOVE "the residual after allocation is 500.00" TO TK-NAME
           MOVE 500.00 TO TK-EXPECTED-N
           MOVE RA-A-RESID-AFTER(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the allocation is a partial settlement" TO TK-NAME
           MOVE "PARTIAL" TO TK-EXPECTED-X
           MOVE RA-A-KIND(1) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the whole receipt is now applied" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE RA-RESIDUAL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 1060-READ-ITEM-1
           MOVE "the ledger shows 500.00 outstanding" TO TK-NAME
           MOVE 500.00 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-PROBE-COL)
           PERFORM TK-ASSERT-NUM
           MOVE "the invoice is partly allocated, not settled"
               TO TK-NAME
           MOVE "PARTIAL" TO TK-EXPECTED-X
           MOVE WS-PROBE-2 TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0260-BUILD-ALLOC
           MOVE WS-R2-ITEM TO RA-SOURCE-ITEM-ID
           MOVE "ZZCS-INV-1" TO RA-A-DOC-NO(1)
           MOVE 480.00 TO RA-A-APPLY(1)
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE "a further 480.00 leaves 20.00" TO TK-NAME
           MOVE 20.00 TO TK-EXPECTED-N
           MOVE RA-A-RESID-AFTER(1) TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           PERFORM 1060-READ-ITEM-1
           MOVE "20.00 is not automatically settled" TO TK-NAME
           MOVE "PARTIAL" TO TK-EXPECTED-X
           MOVE WS-PROBE-2 TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0260-BUILD-ALLOC
           MOVE WS-R5-ITEM TO RA-SOURCE-ITEM-ID
           MOVE "ZZCS-REC-UNCONF" TO RA-SOURCE-DOC
           MOVE "ZZCS-INV-1" TO RA-A-DOC-NO(1)
           MOVE 20.00 TO RA-A-APPLY(1)
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE "an unidentified source is not held" TO TK-NAME
           MOVE 16 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-RA-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

      *>   D-22 - up to GBP 5.00 per receipt may be written off by the
      *>   AR clerk with a reason; a larger difference needs authority
       1300-WRITE-OFF-LIMITS.
           PERFORM 0250-BUILD-RECEIPT
           MOVE 16.00 TO RA-GROSS
           MOVE "ZZARCASH-R4" TO CA-CORRELATION-ID OF WS-RA-CA
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE RA-SOURCE-ITEM-ID TO WS-R4-ITEM
           PERFORM 0260-BUILD-ALLOC
           MOVE WS-R4-ITEM TO RA-SOURCE-ITEM-ID
           MOVE "ZZCS-INV-1" TO RA-A-DOC-NO(1)
           MOVE 1.00 TO RA-A-APPLY(1)
           MOVE 19.00 TO RA-A-WRITEOFF(1)
           MOVE "Short payment" TO RA-A-WO-REASON(1)
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE "a 19.00 write off needs authority" TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-RA-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the D-22 tolerance rule is quoted" TO TK-NAME
           MOVE "RCP0029" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RA-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the clerk tolerance is GBP 5.00" TO TK-NAME
           MOVE 5.00 TO TK-EXPECTED-N
           MOVE RA-WRITEOFF-LIMIT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0260-BUILD-ALLOC
           MOVE WS-R4-ITEM TO RA-SOURCE-ITEM-ID
           MOVE "ZZCS-INV-1" TO RA-A-DOC-NO(1)
           MOVE 16.00 TO RA-A-APPLY(1)
           MOVE 4.00 TO RA-A-WRITEOFF(1)
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE "a short payment is not silently written off"
               TO TK-NAME
           MOVE "RCP0028" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RA-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           PERFORM 0260-BUILD-ALLOC
           MOVE WS-R4-ITEM TO RA-SOURCE-ITEM-ID
           MOVE "ZZCS-INV-1" TO RA-A-DOC-NO(1)
           MOVE 16.00 TO RA-A-APPLY(1)
           MOVE 4.00 TO RA-A-WRITEOFF(1)
           MOVE "Small difference under D-22" TO RA-A-WO-REASON(1)
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE "a 4.00 small difference is written off by the clerk"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-RA-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the write off is separately classified" TO TK-NAME
           MOVE "WRITEOFF" TO TK-EXPECTED-X
           MOVE RA-A-KIND(1) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the invoice is now fully settled" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE RA-A-RESID-AFTER(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 1060-READ-ITEM-1
           MOVE "three contributions settle the 1,200.00 invoice"
               TO TK-NAME
           MOVE "SETTLED" TO TK-EXPECTED-X
           MOVE WS-PROBE-2 TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AR-019 / AC-026 - cross currency needs an explicit
      *>   conversion instruction; a same currency receipt at another
      *>   rate realises an exchange difference
       1400-FOREIGN-CURRENCY.
           PERFORM 0250-BUILD-RECEIPT
           MOVE 1200.00 TO RA-GROSS
           MOVE "USD" TO RA-CURRENCY
           MOVE 1.20 TO RA-FX-RATE
           MOVE "ZZARCASH-R3" TO CA-CORRELATION-ID OF WS-RA-CA
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE RA-SOURCE-ITEM-ID TO WS-R3-ITEM
           MOVE "1,200 USD at 1.20 is 1,000.00 sterling" TO TK-NAME
           MOVE 1000.00 TO TK-EXPECTED-N
           MOVE RA-GROSS-GBP TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           PERFORM 0260-BUILD-ALLOC
           MOVE "PREVIEW" TO CA-FUNCTION OF WS-RA-CA
           MOVE WS-R3-ITEM TO RA-SOURCE-ITEM-ID
           MOVE "ZZCS-INV-2" TO RA-A-DOC-NO(1)
           MOVE 100.00 TO RA-A-APPLY(1)
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE "equal number matching across currencies is blocked"
               TO TK-NAME
           MOVE "RCP0025" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RA-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           PERFORM 0260-BUILD-ALLOC
           MOVE WS-R3-ITEM TO RA-SOURCE-ITEM-ID
           MOVE "ZZCS-INV-3" TO RA-A-DOC-NO(1)
           MOVE 1200.00 TO RA-A-APPLY(1)
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE "the dollar invoice is settled in dollars" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-RA-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the invoice amount is preserved in dollars"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE RA-A-RESID-AFTER(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "960.00 raised against 1,000.00 received is a 40.00 "
               TO TK-NAME
           MOVE -40.00 TO TK-EXPECTED-N
           MOVE RA-A-FX-REALISED(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the realised difference is reported in total"
               TO TK-NAME
           MOVE -40.00 TO TK-EXPECTED-N
           MOVE RA-TOT-FX TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AR-011 / AC-022 - an authorised correction restores the
      *>   residual and never reverses the receipt itself
       1500-UNALLOCATION.
           PERFORM 0260-BUILD-ALLOC
           MOVE "UNALLOC" TO CA-FUNCTION OF WS-RA-CA
           MOVE WS-ALLOC-1 TO RA-ALLOC-ID
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE "an unallocation without a reason is refused"
               TO TK-NAME
           MOVE "RCP0041" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RA-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           PERFORM 0260-BUILD-ALLOC
           MOVE "UNALLOC" TO CA-FUNCTION OF WS-RA-CA
           MOVE WS-ALLOC-1 TO RA-ALLOC-ID
           MOVE "Receipt matched to the wrong invoice"
               TO RA-CORRECT-REASON
           MOVE 20261115 TO RA-CORRECT-DATE
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE "an authorised correction is accepted" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-RA-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "exactly one allocation is reversed" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE RA-REVERSED-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 1060-READ-ITEM-1
           MOVE "the invoice residual is restored to 700.00"
               TO TK-NAME
           MOVE 700.00 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-PROBE-COL)
           PERFORM TK-ASSERT-NUM
           PERFORM 1550-READ-RECEIPT-1
           MOVE "the receipt itself is untouched" TO TK-NAME
           MOVE "CONFIRMED" TO TK-EXPECTED-X
           MOVE WS-PROBE-2 TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the original allocation stays in history" TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-PROBE-COL)
           PERFORM TK-ASSERT-NUM
           PERFORM 0260-BUILD-ALLOC
           MOVE "UNALLOC" TO CA-FUNCTION OF WS-RA-CA
           MOVE WS-ALLOC-1 TO RA-ALLOC-ID
           MOVE "Second attempt" TO RA-CORRECT-REASON
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE "the same allocation cannot be reversed twice"
               TO TK-NAME
           MOVE "RCP0043" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RA-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

       1550-READ-RECEIPT-1.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), (SELECT COUNT(*) FROM "
                  "ar.Allocation a WHERE a.CompanyCode = 'STC' AND "
                  "a.SourceOpenItemId = r2.OpenItemId)), "
                  "(SELECT r.ConfirmationState FROM ar.Receipt r "
                  "WHERE r.CompanyCode = 'STC' AND r.CorrelationId = "
                  "'ZZARCASH-R1') FROM ar.OpenItem r2 WHERE "
                  "r2.CompanyCode = 'STC' AND r2.ItemType = "
                  "'RECEIPT' AND r2.CustomerCode = 'ZZCASH1' AND "
                  "r2.OriginalAmountGBP = -700.00"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0180-READ-ONE.

      *>   AR-012 / AR-020 - balances are never edited; every movement
      *>   is a reasoned, classified and authorised document
       1600-ADJUSTMENT-AUTHORITY.
           PERFORM 1610-BUILD-ADJ
           MOVE 4.00 TO AJ-AMOUNT-GBP
           MOVE "ZZCS-INV-4" TO AJ-TARGET-DOC
           CALL "ARADJUST" USING WS-AJ-CA
           MOVE "a 4.00 difference is inside the clerk tolerance"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE AJ-WITHIN-LIMIT TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-AJ-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           MOVE "the D-22 clerk limit is GBP 5.00" TO TK-NAME
           MOVE 5.00 TO TK-EXPECTED-N
           MOVE AJ-CLERK-LIMIT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "it needs only the minor write off authority"
               TO TK-NAME
           MOVE "AR.ADJUST.WRITEOFF.MINOR" TO TK-EXPECTED-X
           MOVE AJ-REQUIRED-PERM TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the residual falls from 100.00 to 96.00" TO TK-NAME
           MOVE 96.00 TO TK-EXPECTED-N
           MOVE AJ-RESIDUAL-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 1610-BUILD-ADJ
           MOVE 20.00 TO AJ-AMOUNT-GBP
           MOVE "ZZCS-INV-4" TO AJ-TARGET-DOC
           CALL "ARADJUST" USING WS-AJ-CA
           MOVE "a 20.00 write off is above the tolerance" TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-AJ-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-AJ-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "it needs a named approver" TO TK-NAME
           MOVE "ADJ0021" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-AJ-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the approval authority is named" TO TK-NAME
           MOVE "AR.ADJUST.WRITEOFF.APPROVE" TO TK-EXPECTED-X
           MOVE AJ-REQUIRED-PERM TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 1610-BUILD-ADJ
           MOVE 20.00 TO AJ-AMOUNT-GBP
           MOVE "ZZCS-INV-4" TO AJ-TARGET-DOC
           MOVE "ARCLERK" TO AJ-APPROVED-BY
           CALL "ARADJUST" USING WS-AJ-CA
           MOVE "the approver may not be the person who raised it"
               TO TK-NAME
           MOVE "ADJ0022" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-AJ-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-AJ-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           PERFORM 1610-BUILD-ADJ
           MOVE 10.00 TO AJ-AMOUNT-GBP
           MOVE "ZZCS-INV-2" TO AJ-TARGET-DOC
           CALL "ARADJUST" USING WS-AJ-CA
           MOVE "a difference within the stated terms is a discount"
               TO TK-NAME
           MOVE "ADJ0020" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-AJ-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-AJ-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           MOVE "a settlement discount is separately classified"
               TO TK-NAME
           MOVE "SETTLEDISC" TO TK-EXPECTED-X
           MOVE AJ-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 1610-BUILD-ADJ
           MOVE 900.00 TO AJ-AMOUNT-GBP
           MOVE "ZZCS-INV-4" TO AJ-TARGET-DOC
           CALL "ARADJUST" USING WS-AJ-CA
           MOVE "an adjustment cannot exceed the residual" TO TK-NAME
           MOVE "ADJ0016" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-AJ-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-AJ-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           PERFORM 1610-BUILD-ADJ
           MOVE 4.00 TO AJ-AMOUNT-GBP
           MOVE "ZZCS-INV-4" TO AJ-TARGET-DOC
           MOVE SPACES TO AJ-REASON-CODE
           CALL "ARADJUST" USING WS-AJ-CA
           MOVE "there is no balance edit without a reason"
               TO TK-NAME
           MOVE "ADJ0005" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-AJ-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-AJ-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           PERFORM 1610-BUILD-ADJ
           MOVE 4.00 TO AJ-AMOUNT-GBP
           MOVE "ZZCS-INV-4" TO AJ-TARGET-DOC
           MOVE "NOTAPPROVED" TO AJ-REASON-CODE
           CALL "ARADJUST" USING WS-AJ-CA
           MOVE "the reason code must be an approved one" TO TK-NAME
           MOVE "ADJ0015" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-AJ-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-AJ-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

       1610-BUILD-ADJ.
           INITIALIZE WS-AJ-CA
           MOVE "STC" TO CA-COMPANY OF WS-AJ-CA
           MOVE "RDG" TO CA-SITE OF WS-AJ-CA
           MOVE "ARCLERK" TO CA-USER OF WS-AJ-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-AJ-CA
           MOVE "ASSESS" TO CA-FUNCTION OF WS-AJ-CA
           MOVE "ZZCASH1" TO AJ-CUSTOMER
           MOVE "WRITEOFF" TO AJ-ADJ-TYPE
           MOVE "SMALLDIFF" TO AJ-REASON-CODE
           MOVE "Agreed small difference on settlement"
               TO AJ-REASON
           MOVE 20261015 TO AJ-DOC-DATE
           MOVE 20261015 TO AJ-POSTING-DATE.

      *>   AR-014 - a dishonoured receipt puts the debt back and each
      *>   reinstated item reports the amount left open on it
       1700-DISHONOUR-REINSTATES.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT ar.OpenItem (CompanyCode, "
                  "CustomerCode, ItemType, DocumentNumber, "
                  "DocumentDate, DueDate, PostingDate, CurrencyCode, "
                  "FxRate, OriginalAmount, OriginalAmountGBP, "
                  "ResidualAmount, ResidualAmountGBP, VatAmountGBP, "
                  "SettlementStatus, CreatedBy) VALUES "
                  "('STC','ZZCASH1','INVOICE','ZZCS-INV-5',"
                  "'2026-10-01','2026-11-15','2026-10-01','GBP',1,"
                  "400.00,400.00,400.00,400.00,0,'OPEN','ZZARTEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0170-RUN-SQL
           PERFORM 0250-BUILD-RECEIPT
           MOVE 150.00 TO RA-GROSS
           MOVE "ZZARCASH-DH1" TO CA-CORRELATION-ID OF WS-RA-CA
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE RA-SOURCE-ITEM-ID TO WS-DH-ITEM
           MOVE RA-RECEIPT-NO TO WS-DH-NO
           MOVE "a 150.00 receipt is recorded for the dishonour"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-RA-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           PERFORM 0260-BUILD-ALLOC
           MOVE WS-DH-ITEM TO RA-SOURCE-ITEM-ID
           MOVE "ZZCS-INV-5" TO RA-A-DOC-NO(1)
           MOVE 150.00 TO RA-A-APPLY(1)
           MOVE "ZZARCASH-DH2" TO CA-CORRELATION-ID OF WS-RA-CA
           CALL "RCPTALOC" USING WS-RA-CA
           MOVE "it is allocated, leaving 250.00 outstanding"
               TO TK-NAME
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), ResidualAmount), "
                  "SettlementStatus FROM ar.OpenItem WHERE "
                  "CompanyCode = 'STC' AND DocumentNumber = "
                  "'ZZCS-INV-5'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0180-READ-ONE
           MOVE 250.00 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-PROBE-COL)
           MOVE CA-MESSAGE OF WS-RA-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           INITIALIZE WS-DH-CA
           MOVE "STC" TO CA-COMPANY OF WS-DH-CA
           MOVE "RDG" TO CA-SITE OF WS-DH-CA
           MOVE "ARCLERK" TO CA-USER OF WS-DH-CA
           MOVE 20261020 TO CA-BUSINESS-DATE OF WS-DH-CA
           MOVE "ZZARCASH-DH3" TO CA-CORRELATION-ID OF WS-DH-CA
           MOVE "DISHONR" TO CA-FUNCTION OF WS-DH-CA
           MOVE WS-DH-NO TO DH-RECEIPT-NO
           MOVE "BANK" TO DH-SOURCE
           MOVE 20261020 TO DH-DISHONOUR-DATE
           MOVE 20261020 TO DH-POSTING-DATE
           MOVE "Returned unpaid - refer to drawer" TO DH-REASON
           CALL "DISHONOR" USING WS-DH-CA
           MOVE "the returned receipt is dishonoured" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-DH-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-DH-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the item it paid is named" TO TK-NAME
           MOVE "ZZCS-INV-5" TO TK-EXPECTED-X
           MOVE DH-I-DOC-NO (1) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the 150.00 it had paid is reinstated" TO TK-NAME
           MOVE 150.00 TO TK-EXPECTED-N
           MOVE DH-I-AMOUNT (1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the item is reported as 400.00 open again"
               TO TK-NAME
           MOVE 400.00 TO TK-EXPECTED-N
           MOVE DH-I-RESID-AFTER (1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), ResidualAmount), "
                  "SettlementStatus FROM ar.OpenItem WHERE "
                  "CompanyCode = 'STC' AND DocumentNumber = "
                  "'ZZCS-INV-5'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0180-READ-ONE
           MOVE "the reported residual is the ledger residual"
               TO TK-NAME
           MOVE DH-I-RESID-AFTER (1) TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-PROBE-COL)
           PERFORM TK-ASSERT-NUM
           MOVE "the reinstated item is open again" TO TK-NAME
           MOVE "OPEN" TO TK-EXPECTED-X
           MOVE WS-PROBE-2 TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       0900-SKIP-EVERYTHING.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 45
               MOVE "database not reachable - STERP_ODBC / SQL Server"
                   TO TK-DETAIL
               MOVE "TARCASH receipts and allocation" TO TK-NAME
               PERFORM TK-SKIP
           END-PERFORM.

       COPY TESTKITP.
