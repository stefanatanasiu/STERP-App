      *>================================================================
      *> PROGRAM-ID : TARCOLL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR) UNIT TESTS
      *> PURPOSE    : THE COLLECTIONS SIDE OF THE SALES LEDGER -
      *>              BADDEBT (VAT BAD DEBT RELIEF) AND DDMAND
      *>              (BACS DIRECT DEBIT MANDATES AND COLLECTIONS).
      *>              WORKED NUMBERS FROM THE DEBTORS SPECIFICATION
      *>              SECTION 11 AND SCENARIOS AC-122 / AC-123: a
      *>              1,200.00 invoice (1,000.00 + 200.00 VAT) due
      *>              31/01/2027 - relief of 200.00 first claimable
      *>              in a return covering 31/07/2027 and a later
      *>              600.00 receipt repaying 100.00; ten working
      *>              days of advance notice; Bacs Standard 18 codes
      *>              0N, 01, 17, 0C.  THE SUITE BUILDS ITS OWN
      *>              ZZCOLL / ZZBD / ZZDD KEYS AND CLEANS THEM UP.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: AR-025, AR-026, D-16   AC: AC-122, AC-123
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TARCOLL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY BADDEBTCA REPLACING ==DFHCOMMAREA== BY ==WS-BD-CA==.
       COPY DDMANDCA REPLACING ==DFHCOMMAREA== BY ==WS-DM-CA==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "Y".
       01  WS-PROBE.
           05  WS-PROBE-COL            PIC X(30).
           05  WS-PROBE-2              PIC X(30).
       01  WS-I                        PIC S9(4) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TARCOLL collections and relief" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-EVERYTHING
           ELSE
               PERFORM 0150-CLEAN-UP
               PERFORM 0160-BUILD-FIXTURE
               PERFORM 1000-SIX-MONTH-RULE
               PERFORM 1100-RELIEF-CONDITIONS
               PERFORM 1200-NO-DOUBLE-RELIEF
               PERFORM 1300-REPAY-ON-RECEIPT
               PERFORM 2000-MANDATE-EDITS
               PERFORM 2100-LODGE-AND-ACTIVATE
               PERFORM 2200-ADVANCE-NOTICE
               PERFORM 2300-COLLECTION-RULES
               PERFORM 2400-CANCELLED-MANDATE
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
           MOVE 0 TO SQLIO-PARM-CNT SQLIO-COL-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR
               DISPLAY "# fixture SQL: "
                       FUNCTION TRIM(SQLIO-ERRMSG) UPON SYSOUT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

       0150-CLEAN-UP.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; DELETE ar.DirectDebitCollection "
                  "WHERE CompanyCode = 'STC' AND (CustomerCode LIKE "
                  "'ZZCOLL%' OR CollectionRef LIKE 'ZZDD%'); DELETE "
                  "ar.DirectDebitMandate WHERE CompanyCode = 'STC' "
                  "AND (CustomerCode LIKE 'ZZCOLL%' OR "
                  "MandateReference LIKE 'ZZDD%'); DELETE "
                  "ar.BadDebtRelief WHERE CompanyCode = 'STC' AND "
                  "(CustomerCode LIKE 'ZZCOLL%' OR InvoiceNumber "
                  "LIKE 'ZZBD%'); DELETE fin.VatTransaction WHERE "
                  "CompanyCode = 'STC' AND (DocumentRef LIKE 'ZZBD%' "
                  "OR PartyCode LIKE 'ZZCOLL%'); DELETE ar.OpenItem "
                  "WHERE CompanyCode = 'STC' AND CustomerCode LIKE "
                  "'ZZCOLL%'; DELETE core.ExceptionItem WHERE "
                  "CompanyCode = 'STC' AND DocumentRef LIKE 'ZZDD%'; "
                  "DELETE ar.Customer WHERE CompanyCode = 'STC' AND "
                  "CustomerCode LIKE 'ZZCOLL%'; DELETE core.AuditLog "
                  "WHERE EntityKey LIKE 'ZZDD%' OR EntityKey LIKE "
                  "'ZZCOLL%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0170-RUN-SQL.

      *>   one Direct Debit payer and one debtor whose debt goes bad.
      *>   ZZBD-INV-1 is the specification F5 invoice, ZZBD-INV-2 has
      *>   not been written off, ZZBD-INV-3 is already relieved and
      *>   ZZDD-MANDATE-9 is an instruction that has lapsed.
       0160-BUILD-FIXTURE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT ar.Customer (CompanyCode, "
                  "CustomerCode, LegalName, LegalForm, VatLocation, "
                  "CountryCode, CurrencyCode, AllowedCurrencies, "
                  "CollectionMethod, Status, SanctionsStatus, "
                  "CreatedBy) VALUES ('STC','ZZCOLL1','Zed "
                  "Collections Payer Limited','LTD','GB','GB','GBP',"
                  "'GBP','DD','ACTIVE','CLEAR','ZZARTEST'), ('STC',"
                  "'ZZCOLL2','Zed Bad Debt Limited','LTD','GB','GB',"
                  "'GBP','GBP','BACS','ACTIVE','CLEAR','ZZARTEST'); "
                  "INSERT ar.OpenItem (CompanyCode, CustomerCode, "
                  "ItemType, DocumentNumber, DocumentDate, DueDate, "
                  "PostingDate, CurrencyCode, FxRate, "
                  "OriginalAmount, OriginalAmountGBP, "
                  "ResidualAmount, ResidualAmountGBP, VatAmountGBP, "
                  "WrittenOff, SettlementStatus, CreatedBy) SELECT "
                  "'STC','ZZCOLL2','INVOICE', v.d,'2026-12-31',"
                  "'2027-01-31','2026-12-31','GBP',1,1200,1200,1200,"
                  "1200,200, v.w,'OPEN','ZZARTEST' FROM (VALUES "
                  "('ZZBD-INV-1',1),('ZZBD-INV-2',0),"
                  "('ZZBD-INV-3',1)) v(d, w); "
                  "INSERT ar.DirectDebitMandate (CompanyCode, "
                  "CustomerCode, MandateReference, "
                  "ServiceUserNumber, PayerName, SortCode, "
                  "AccountNumber, SignedDate, LodgedDate, "
                  "AdvanceNoticeDays, Status, CreatedBy) VALUES "
                  "('STC','ZZCOLL2','ZZDD-MANDATE-9','912345','Zed "
                  "Bad Debt Limited','089999','66374958',"
                  "'2024-01-10','2024-01-20',10,'EXPIRED',"
                  "'ZZARTEST'); INSERT ar.BadDebtRelief "
                  "(CompanyCode, CustomerCode, OpenItemId, "
                  "InvoiceNumber, SupplyDate, DueDate, "
                  "EligibleFromDate, WrittenOffDate, WrittenOffGBP, "
                  "VatAccountedGBP, ReliefClaimedGBP, "
                  "ClaimVatPeriodKey, Status, CreatedBy) SELECT "
                  "'STC','ZZCOLL2', o.OpenItemId,'ZZBD-INV-3',"
                  "'2026-12-31','2027-01-31','2027-07-31',"
                  "'2027-09-30',1200,200,200,'2027Q3','CLAIMED',"
                  "'ZZARTEST' FROM ar.OpenItem o WHERE "
                  "o.CompanyCode = 'STC' AND o.DocumentNumber = "
                  "'ZZBD-INV-3'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0170-RUN-SQL.

      *>   an assessment of the F5 invoice in the September return
       0200-BUILD-ASSESS.
           INITIALIZE WS-BD-CA
           MOVE "STC" TO CA-COMPANY OF WS-BD-CA
           MOVE "RDG" TO CA-SITE OF WS-BD-CA
           MOVE "ARCLERK" TO CA-USER OF WS-BD-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-BD-CA
           MOVE "ASSESS" TO CA-FUNCTION OF WS-BD-CA
           MOVE "ZZBD-INV-1" TO BD-INVOICE-NO
           MOVE "Y" TO BD-VAT-PAID
           MOVE "N" TO BD-SOLD-FACTORED
           MOVE 20270930 TO BD-RETURN-END-DATE
           MOVE "2027Q3" TO BD-VAT-PERIOD-KEY.

       0210-BUILD-REPAY.
           PERFORM 0200-BUILD-ASSESS
           MOVE "REPAY" TO CA-FUNCTION OF WS-BD-CA
           MOVE "ZZBD-INV-3" TO BD-INVOICE-NO
           MOVE 20271231 TO BD-RETURN-END-DATE
           MOVE "2027Q4" TO BD-REPAY-PERIOD.

       0300-BD-RC.
           MOVE CA-RETURN-CODE OF WS-BD-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-BD-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

       0310-BD-REASON.
           MOVE CA-REASON-CODE OF WS-BD-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-BD-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

       0320-BD-BLOCK.
           MOVE BD-BLOCK-REASON(1:25) TO TK-ACTUAL-X
           MOVE BD-BLOCK-REASON TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

       0330-BD-ELIGIBLE.
           MOVE BD-ELIGIBLE TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-BD-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

      *>   AR-026 / AC-123 - relief is first claimable six months
      *>   after the later of supply and due date, so a 31/01/2027
      *>   debt is blocked in the June return and allowed in September
       1000-SIX-MONTH-RULE.
           PERFORM 0200-BUILD-ASSESS
           MOVE 20270630 TO BD-RETURN-END-DATE
           CALL "BADDEBT" USING WS-BD-CA
           MOVE "the June return is refused" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           PERFORM 0300-BD-RC
           MOVE "a blocked claim reports BDR0010" TO TK-NAME
           MOVE "BDR0010" TO TK-EXPECTED-X
           PERFORM 0310-BD-REASON
           MOVE "the block says not yet six months" TO TK-NAME
           MOVE "The debt is not yet six m" TO TK-EXPECTED-X
           PERFORM 0320-BD-BLOCK
           MOVE "no relief is offered when blocked" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE BD-RELIEF-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "relief is claimable from 31/07/2027" TO TK-NAME
           MOVE 20270731 TO TK-EXPECTED-N
           MOVE BD-ELIGIBLE-FROM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0200-BUILD-ASSESS
           MOVE 20270430 TO BD-RETURN-END-DATE
           CALL "BADDEBT" USING WS-BD-CA
           MOVE "three months overdue is not enough" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           PERFORM 0330-BD-ELIGIBLE
           PERFORM 0200-BUILD-ASSESS
           CALL "BADDEBT" USING WS-BD-CA
           MOVE "the September return is accepted" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           PERFORM 0300-BD-RC
           MOVE "the debt is eligible for relief" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           PERFORM 0330-BD-ELIGIBLE
           MOVE "the relief is the 200.00 VAT element" TO TK-NAME
           MOVE 200.00 TO TK-EXPECTED-N
           MOVE BD-RELIEF-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the relief is reclaimed in Box 4" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE BD-VAT-BOX TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AR-026 - the other conditions: the output VAT must have
      *>   been accounted for and paid, the debt must be written off
      *>   in the VAT account and must not have been sold on
       1100-RELIEF-CONDITIONS.
           PERFORM 0200-BUILD-ASSESS
           MOVE "ZZBD-INV-2" TO BD-INVOICE-NO
           CALL "BADDEBT" USING WS-BD-CA
           MOVE "a debt not written off is refused" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           PERFORM 0300-BD-RC
           MOVE "the block names the write off" TO TK-NAME
           MOVE "The debt has not been wri" TO TK-EXPECTED-X
           PERFORM 0320-BD-BLOCK
           PERFORM 0200-BUILD-ASSESS
           MOVE "Y" TO BD-SOLD-FACTORED
           CALL "BADDEBT" USING WS-BD-CA
           MOVE "a factored debt cannot be relieved" TO TK-NAME
           MOVE "The debt has been sold or" TO TK-EXPECTED-X
           PERFORM 0320-BD-BLOCK
           PERFORM 0200-BUILD-ASSESS
           MOVE "N" TO BD-VAT-PAID
           CALL "BADDEBT" USING WS-BD-CA
           MOVE "unpaid output VAT blocks the claim" TO TK-NAME
           MOVE "The output VAT on the sup" TO TK-EXPECTED-X
           PERFORM 0320-BD-BLOCK.

      *>   AR-026 - relief already taken is not offered again
       1200-NO-DOUBLE-RELIEF.
           PERFORM 0200-BUILD-ASSESS
           MOVE "ZZBD-INV-3" TO BD-INVOICE-NO
           CALL "BADDEBT" USING WS-BD-CA
           MOVE "the earlier 200.00 claim is found" TO TK-NAME
           MOVE 200.00 TO TK-EXPECTED-N
           MOVE BD-RELIEF-CLAIMED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "relief is not claimed a second time" TO TK-NAME
           MOVE "There is no unclaimed VAT" TO TK-EXPECTED-X
           PERFORM 0320-BD-BLOCK
           MOVE "and nothing more is offered" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE BD-RELIEF-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AR-026 / AC-123 - a later receipt repays the relief in
      *>   proportion: 600.00 of a 1,200.00 debt repays 100.00
       1300-REPAY-ON-RECEIPT.
           PERFORM 0200-BUILD-ASSESS
           MOVE "REPAY" TO CA-FUNCTION OF WS-BD-CA
           MOVE 600.00 TO BD-RECEIPT-GBP
           CALL "BADDEBT" USING WS-BD-CA
           MOVE "nothing is repaid if nothing was claimed"
               TO TK-NAME
           MOVE "BDR0020" TO TK-EXPECTED-X
           PERFORM 0310-BD-REASON
           PERFORM 0210-BUILD-REPAY
           CALL "BADDEBT" USING WS-BD-CA
           MOVE "a repayment needs a positive receipt" TO TK-NAME
           MOVE "BDR0021" TO TK-EXPECTED-X
           PERFORM 0310-BD-REASON
           PERFORM 0210-BUILD-REPAY
           MOVE 600.00 TO BD-RECEIPT-GBP
           CALL "BADDEBT" USING WS-BD-CA
           MOVE "the 600.00 receipt is accepted" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           PERFORM 0300-BD-RC
           MOVE "200.00 x 600.00 / 1,200.00 repays 100.00"
               TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE BD-REPAY-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the repayment goes through Box 1" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE BD-VAT-BOX TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *>   a signed instruction with the payer's branch and account
       2050-BUILD-LODGE.
           INITIALIZE WS-DM-CA
           MOVE "STC" TO CA-COMPANY OF WS-DM-CA
           MOVE "RDG" TO CA-SITE OF WS-DM-CA
           MOVE "CREDMGR" TO CA-USER OF WS-DM-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-DM-CA
           MOVE "LODGE" TO CA-FUNCTION OF WS-DM-CA
           MOVE "ZZCOLL1" TO DM-CUSTOMER
           MOVE "ZZDD-MANDATE-1" TO DM-MANDATE-REF
           MOVE "912345" TO DM-SERVICE-USER-NO
           MOVE "Zed Collections Payer Limited" TO DM-PAYER-NAME
           MOVE "089999" TO DM-SORT-CODE
           MOVE "66374958" TO DM-ACCOUNT-NO
           MOVE 20261012 TO DM-SIGNED-DATE.

       2060-BUILD-COLLECT.
           INITIALIZE WS-DM-CA
           MOVE "STC" TO CA-COMPANY OF WS-DM-CA
           MOVE "RDG" TO CA-SITE OF WS-DM-CA
           MOVE "ARCLERK" TO CA-USER OF WS-DM-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-DM-CA
           MOVE "SCHEDULE" TO CA-FUNCTION OF WS-DM-CA
           MOVE "ZZCOLL1" TO DM-CUSTOMER
           MOVE 20261015 TO DM-NOTICE-SENT
           MOVE 500.00 TO DM-AMOUNT-GBP.

       2070-COLLECT-BY-REF.
           PERFORM 2060-BUILD-COLLECT
           MOVE SPACES TO DM-CUSTOMER
           MOVE 20261201 TO DM-REQUEST-DATE.

       2010-DM-RC.
           MOVE CA-RETURN-CODE OF WS-DM-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-DM-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

       2020-DM-REASON.
           MOVE CA-REASON-CODE OF WS-DM-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-DM-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

       2030-DM-FIELD.
           MOVE CA-MESSAGE OF WS-DM-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

      *>   AR-025 / D-16 - the paper instruction is edited before it
      *>   is offered to the paying bank: a six digit sorting code,
      *>   an eight digit account number and the modulus check
       2000-MANDATE-EDITS.
           PERFORM 2050-BUILD-LODGE
           MOVE "12345" TO DM-SORT-CODE
           CALL "DDMAND" USING WS-DM-CA
           MOVE "a sorting code must be six digits" TO TK-NAME
           MOVE "DDM0011" TO TK-EXPECTED-X
           PERFORM 2020-DM-REASON
           PERFORM 2050-BUILD-LODGE
           MOVE "1234567" TO DM-ACCOUNT-NO
           CALL "DDMAND" USING WS-DM-CA
           MOVE "an account must be eight digits" TO TK-NAME
           MOVE "DDM0012" TO TK-EXPECTED-X
           PERFORM 2020-DM-REASON
           PERFORM 2050-BUILD-LODGE
           MOVE "66374959" TO DM-ACCOUNT-NO
           CALL "DDMAND" USING WS-DM-CA
           MOVE "the details must pass the modulus check"
               TO TK-NAME
           MOVE "DDM0013" TO TK-EXPECTED-X
           PERFORM 2020-DM-REASON.

      *>   AR-025 / AC-122 - lodgement is an AUDDIS 0N, and the
      *>   mandate is only collectable once it is lodged and the
      *>   Direct Debit Guarantee has gone to the payer
       2100-LODGE-AND-ACTIVATE.
           PERFORM 2050-BUILD-LODGE
           CALL "DDMAND" USING WS-DM-CA
           MOVE "the mandate waits to be lodged" TO TK-NAME
           MOVE "PENDING" TO TK-EXPECTED-X
           MOVE DM-STATUS TO TK-ACTUAL-X
           PERFORM 2030-DM-FIELD
           MOVE "a new instruction is an AUDDIS 0N" TO TK-NAME
           MOVE "0N" TO TK-EXPECTED-X
           MOVE DM-TXN-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the baseline advance notice is 10 days" TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE DM-ADVANCE-DAYS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 2050-BUILD-LODGE
           MOVE "ZZCOLL2" TO DM-CUSTOMER
           MOVE "ZZDD-MANDATE-2" TO DM-MANDATE-REF
           MOVE "Zed Bad Debt Limited" TO DM-PAYER-NAME
           MOVE 5 TO DM-ADVANCE-DAYS
           CALL "DDMAND" USING WS-DM-CA
           MOVE "a shorter notice needs written agreement"
               TO TK-NAME
           MOVE "DDM0021" TO TK-EXPECTED-X
           PERFORM 2020-DM-REASON
           MOVE "the baseline notice is applied instead" TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE DM-ADVANCE-DAYS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 2050-BUILD-LODGE
           MOVE "ACTIVATE" TO CA-FUNCTION OF WS-DM-CA
           CALL "DDMAND" USING WS-DM-CA
           MOVE "the Guarantee goes out before activation"
               TO TK-NAME
           MOVE "DDM0031" TO TK-EXPECTED-X
           PERFORM 2020-DM-REASON
           PERFORM 2050-BUILD-LODGE
           MOVE "ACTIVATE" TO CA-FUNCTION OF WS-DM-CA
           MOVE "Y" TO DM-GUARANTEE-SENT
           MOVE 20261014 TO DM-LODGED-DATE
           CALL "DDMAND" USING WS-DM-CA
           MOVE "a lodged mandate is activated" TO TK-NAME
           MOVE "ACTIVE" TO TK-EXPECTED-X
           MOVE DM-STATUS TO TK-ACTUAL-X
           PERFORM 2030-DM-FIELD.

      *>   AR-025 - advance notice of the amount and the date, ten
      *>   working days ahead on the Bacs calendar
       2200-ADVANCE-NOTICE.
           PERFORM 2060-BUILD-COLLECT
           MOVE "NOTICE" TO CA-FUNCTION OF WS-DM-CA
           MOVE 0 TO DM-NOTICE-SENT
           CALL "DDMAND" USING WS-DM-CA
           MOVE "ten working days on gives 29/10/2026" TO TK-NAME
           MOVE 20261029 TO TK-EXPECTED-N
           MOVE DM-EARLIEST-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AR-025 / AC-122 - no money is taken before the advance
      *>   notice has run its course, and collection falls on a Bacs
      *>   processing day.  Bacs Standard 18: 01 first, 17 regular.
       2300-COLLECTION-RULES.
           PERFORM 2060-BUILD-COLLECT
           MOVE 0 TO DM-NOTICE-SENT
           MOVE 20261103 TO DM-REQUEST-DATE
           CALL "DDMAND" USING WS-DM-CA
           MOVE "no collection without an advance notice"
               TO TK-NAME
           MOVE "DDM0052" TO TK-EXPECTED-X
           PERFORM 2020-DM-REASON
           PERFORM 2060-BUILD-COLLECT
           MOVE 20261020 TO DM-REQUEST-DATE
           CALL "DDMAND" USING WS-DM-CA
           MOVE "a collection inside the notice is held"
               TO TK-NAME
           MOVE "DDM0053" TO TK-EXPECTED-X
           PERFORM 2020-DM-REASON
           MOVE "an early collection is a hold, not an error"
               TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           PERFORM 2010-DM-RC
           PERFORM 2060-BUILD-COLLECT
           MOVE 20261103 TO DM-REQUEST-DATE
           MOVE "ZZDD-COLL-1" TO DM-COLLECT-REF
           CALL "DDMAND" USING WS-DM-CA
           MOVE "the first collection is planned" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           PERFORM 2010-DM-RC
           MOVE "it is flagged as the first collection" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE DM-FIRST-COLLECT TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a first collection is Bacs code 01" TO TK-NAME
           MOVE "01" TO TK-EXPECTED-X
           MOVE DM-TXN-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "Bacs processes it on 03/11/2026" TO TK-NAME
           MOVE 20261103 TO TK-EXPECTED-N
           MOVE DM-PROCESS-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 2060-BUILD-COLLECT
           MOVE 20261201 TO DM-REQUEST-DATE
           MOVE "ZZDD-COLL-2" TO DM-COLLECT-REF
           CALL "DDMAND" USING WS-DM-CA
           MOVE "the mandate has now been collected on" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE DM-FIRST-COLLECT TO TK-ACTUAL-X
           PERFORM 2030-DM-FIELD
           MOVE "a regular collection is Bacs code 17" TO TK-NAME
           MOVE "17" TO TK-EXPECTED-X
           MOVE DM-TXN-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AR-025 - cancellation is an AUDDIS 0C with a reason, and
      *>   a cancelled or lapsed instruction cannot be collected on
       2400-CANCELLED-MANDATE.
           PERFORM 2050-BUILD-LODGE
           MOVE "CANCEL" TO CA-FUNCTION OF WS-DM-CA
           CALL "DDMAND" USING WS-DM-CA
           MOVE "a cancellation needs a reason" TO TK-NAME
           MOVE "DDM0003" TO TK-EXPECTED-X
           PERFORM 2020-DM-REASON
           PERFORM 2050-BUILD-LODGE
           MOVE "CANCEL" TO CA-FUNCTION OF WS-DM-CA
           MOVE "ADDACS 1 - cancelled by the payer at the bank"
               TO DM-CANCEL-REASON
           CALL "DDMAND" USING WS-DM-CA
           MOVE "the instruction is now CANCELLED" TO TK-NAME
           MOVE "CANCELLED" TO TK-EXPECTED-X
           MOVE DM-STATUS TO TK-ACTUAL-X
           PERFORM 2030-DM-FIELD
           MOVE "it is withdrawn as an AUDDIS 0C" TO TK-NAME
           MOVE "0C" TO TK-EXPECTED-X
           MOVE DM-TXN-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 2070-COLLECT-BY-REF
           MOVE "ZZDD-MANDATE-1" TO DM-MANDATE-REF
           MOVE "ZZDD-COLL-4" TO DM-COLLECT-REF
           CALL "DDMAND" USING WS-DM-CA
           MOVE "a cancelled mandate is not collected on"
               TO TK-NAME
           MOVE "DDM0050" TO TK-EXPECTED-X
           PERFORM 2020-DM-REASON
           PERFORM 2070-COLLECT-BY-REF
           MOVE "ZZDD-MANDATE-9" TO DM-MANDATE-REF
           MOVE "ZZDD-COLL-5" TO DM-COLLECT-REF
           CALL "DDMAND" USING WS-DM-CA
           MOVE "a lapsed mandate is not collected on" TO TK-NAME
           MOVE "DDM0050" TO TK-EXPECTED-X
           PERFORM 2020-DM-REASON
           PERFORM 2070-COLLECT-BY-REF
           MOVE "ZZDD-MANDATE-X" TO DM-MANDATE-REF
           CALL "DDMAND" USING WS-DM-CA
           MOVE "an unknown mandate is not found" TO TK-NAME
           MOVE "DDM0014" TO TK-EXPECTED-X
           PERFORM 2020-DM-REASON.

       0900-SKIP-EVERYTHING.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 45
               MOVE "database not reachable - STERP_ODBC / SQL Server"
                   TO TK-DETAIL
               MOVE "TARCOLL collections and relief" TO TK-NAME
               PERFORM TK-SKIP
           END-PERFORM.

       COPY TESTKITP.
