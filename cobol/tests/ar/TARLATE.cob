      *>================================================================
      *> PROGRAM-ID : TARLATE
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR) UNIT TESTS
      *> PURPOSE    : LATEINT - STATUTORY INTEREST AND FIXED SUM
      *>              COMPENSATION UNDER THE LATE PAYMENT OF
      *>              COMMERCIAL DEBTS (INTEREST) ACT 1998.  USES THE
      *>              WORKED NUMBERS OF ACCEPTANCE SCENARIO AC-121 AND
      *>              THE DEBTORS SPECIFICATION SECTION 11:
      *>                5,000.00 x 12% x 30 / 365 = 49.32
      *>                compensation 40 / 70 / 100 by debt band
      *>              LATEINT OWNS NO TRANSACTION OF ITS OWN, SO THE
      *>              LEDGER FIXTURES ARE WRAPPED IN BEGN ... ROLB AND
      *>              NOTHING IS LEFT BEHIND.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: AR-024, D-25, D-28   AC: AC-121
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TARLATE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY LATEINTCA REPLACING ==DFHCOMMAREA== BY ==WS-LI-CA==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "Y".
       01  WS-PROBE.
           05  WS-PROBE-COL            PIC X(12).
           05  WS-PROBE-2              PIC X(12).
       01  WS-ROW.
           05  WS-R1                   PIC X(30).
           05  WS-R2                   PIC X(30).
       01  WS-I                        PIC S9(4) VALUE 0.
       01  WS-SPREAD                   PIC S9(5)V9(4) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TARLATE late payment interest" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           PERFORM 1000-AC121-WORKED-EXAMPLE
           PERFORM 1100-COMPENSATION-BANDS
           PERFORM 1200-DAY-COUNT-BASIS
           PERFORM 1300-NON-QUALIFYING-DEBT
           PERFORM 1400-NOTHING-TO-CHARGE
           PERFORM 1500-WAIVER-NEEDS-A-REASON
           PERFORM 1600-REFERENCE-RATE-BASIS
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-LEDGER-TESTS
           ELSE
               PERFORM 1700-UNKNOWN-INVOICE
               PERFORM 1800-CLAIM-FROM-THE-LEDGER
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
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "N" TO WS-DB-UP
           END-IF.

      *>   a bare claim, principal and days supplied by the caller
       0200-BUILD-CLAIM.
           INITIALIZE WS-LI-CA
           MOVE "STC" TO CA-COMPANY OF WS-LI-CA
           MOVE "RDG" TO CA-SITE OF WS-LI-CA
           MOVE "ARCLERK" TO CA-USER OF WS-LI-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-LI-CA
           MOVE "CALC" TO CA-FUNCTION OF WS-LI-CA
           MOVE "C0004" TO LI-CUSTOMER
           MOVE 20261015 TO LI-CALC-TO-DATE
           MOVE 4.00 TO LI-REF-RATE-PCT
           MOVE 30 TO LI-DAYS-OVERDUE.

      *>   AC-121 - a 5,000.00 debt paid 30 days late while the
      *>   reference rate is 4.00%: interest 49.32, compensation 70.00
       1000-AC121-WORKED-EXAMPLE.
           PERFORM 0200-BUILD-CLAIM
           MOVE 5000.00 TO LI-PRINCIPAL
           CALL "LATEINT" USING WS-LI-CA
           MOVE "the statutory claim calculates" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-LI-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-LI-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the reference rate is 4.00%" TO TK-NAME
           MOVE 4.00 TO TK-EXPECTED-N
           MOVE LI-REF-RATE-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the statutory margin is 8%" TO TK-NAME
           MOVE 8.00 TO TK-EXPECTED-N
           MOVE LI-MARGIN-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "base plus 8% gives 12.00%" TO TK-NAME
           MOVE 12.00 TO TK-EXPECTED-N
           MOVE LI-TOTAL-RATE-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "interest 5,000 x 12% x 30 / 365 is 49.32" TO TK-NAME
           MOVE 49.32 TO TK-EXPECTED-N
           MOVE LI-INTEREST TO TK-ACTUAL-N
           MOVE LI-EXPLANATION TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "compensation on a 5,000.00 debt is 70.00" TO TK-NAME
           MOVE 70.00 TO TK-EXPECTED-N
           MOVE LI-COMPENSATION TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the debt falls in compensation band 2" TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           MOVE LI-COMP-BAND TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the total claim is 119.32" TO TK-NAME
           MOVE 119.32 TO TK-EXPECTED-N
           MOVE LI-TOTAL-CLAIM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "neither amount carries VAT" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE LI-VAT-BEARING TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the calculation is explained on the claim" TO TK-NAME
           IF LI-EXPLANATION NOT = SPACES
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>   AR-024 / AC-121 - 40 under 1,000, 70 from 1,000 to
      *>   9,999.99 and 100 from 10,000, per invoice
       1100-COMPENSATION-BANDS.
           PERFORM 0200-BUILD-CLAIM
           MOVE 900.00 TO LI-PRINCIPAL
           CALL "LATEINT" USING WS-LI-CA
           MOVE "a 900.00 debt attracts 40.00" TO TK-NAME
           MOVE 40.00 TO TK-EXPECTED-N
           MOVE LI-COMPENSATION TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "900.00 is compensation band 1" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE LI-COMP-BAND TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0200-BUILD-CLAIM
           MOVE 999.99 TO LI-PRINCIPAL
           CALL "LATEINT" USING WS-LI-CA
           MOVE "999.99 is still the 40.00 band" TO TK-NAME
           MOVE 40.00 TO TK-EXPECTED-N
           MOVE LI-COMPENSATION TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0200-BUILD-CLAIM
           MOVE 1000.00 TO LI-PRINCIPAL
           CALL "LATEINT" USING WS-LI-CA
           MOVE "1,000.00 exactly attracts 70.00" TO TK-NAME
           MOVE 70.00 TO TK-EXPECTED-N
           MOVE LI-COMPENSATION TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "1,000.00 is compensation band 2" TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           MOVE LI-COMP-BAND TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0200-BUILD-CLAIM
           MOVE 9999.99 TO LI-PRINCIPAL
           CALL "LATEINT" USING WS-LI-CA
           MOVE "9,999.99 is the top of the 70.00 band" TO TK-NAME
           MOVE 70.00 TO TK-EXPECTED-N
           MOVE LI-COMPENSATION TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0200-BUILD-CLAIM
           MOVE 10000.00 TO LI-PRINCIPAL
           CALL "LATEINT" USING WS-LI-CA
           MOVE "10,000.00 exactly attracts 100.00" TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE LI-COMPENSATION TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "10,000.00 is compensation band 3" TO TK-NAME
           MOVE 3 TO TK-EXPECTED-N
           MOVE LI-COMP-BAND TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0200-BUILD-CLAIM
           MOVE 12000.00 TO LI-PRINCIPAL
           CALL "LATEINT" USING WS-LI-CA
           MOVE "a 12,000.00 debt attracts 100.00" TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE LI-COMPENSATION TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AR-024 - simple interest on a 365 day basis, running from
      *>   the day after the due date
       1200-DAY-COUNT-BASIS.
           PERFORM 0200-BUILD-CLAIM
           MOVE 10000.00 TO LI-PRINCIPAL
           MOVE 365 TO LI-DAYS-OVERDUE
           CALL "LATEINT" USING WS-LI-CA
           MOVE "a full year at 12% on 10,000 is 1,200.00" TO TK-NAME
           MOVE 1200.00 TO TK-EXPECTED-N
           MOVE LI-INTEREST TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0200-BUILD-CLAIM
           MOVE 5000.00 TO LI-PRINCIPAL
           MOVE 1 TO LI-DAYS-OVERDUE
           CALL "LATEINT" USING WS-LI-CA
           MOVE "one day on 5,000 at 12% is 1.64" TO TK-NAME
           MOVE 1.64 TO TK-EXPECTED-N
           MOVE LI-INTEREST TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0200-BUILD-CLAIM
           MOVE 5000.00 TO LI-PRINCIPAL
           MOVE 0 TO LI-DAYS-OVERDUE
           CALL "LATEINT" USING WS-LI-CA
           MOVE "on the due date no interest has accrued" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE LI-INTEREST TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "and no compensation is due yet" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE LI-COMPENSATION TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0200-BUILD-CLAIM
           MOVE 5000.00 TO LI-PRINCIPAL
           MOVE -5 TO LI-DAYS-OVERDUE
           CALL "LATEINT" USING WS-LI-CA
           MOVE "a debt not yet due shows no days overdue" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE LI-DAYS-OVERDUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "and attracts no statutory interest" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE LI-INTEREST TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AR-024 - the Act does not apply where the contract provides
      *>   a substantial alternative remedy, nor to a non qualifying
      *>   debt; both are warnings, not errors
       1300-NON-QUALIFYING-DEBT.
           PERFORM 0200-BUILD-CLAIM
           MOVE 5000.00 TO LI-PRINCIPAL
           MOVE "N" TO LI-QUALIFYING
           CALL "LATEINT" USING WS-LI-CA
           MOVE "a non qualifying debt warns" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-LI-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-LI-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the warning names the qualifying rule" TO TK-NAME
           MOVE "LPI0005" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-LI-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "no interest on a non qualifying debt" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE LI-INTEREST TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "no compensation on a non qualifying debt" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE LI-COMPENSATION TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0200-BUILD-CLAIM
           MOVE 5000.00 TO LI-PRINCIPAL
           MOVE "Y" TO LI-ALT-REMEDY
           CALL "LATEINT" USING WS-LI-CA
           MOVE "a substantial alternative remedy warns" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-LI-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-LI-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the alternative remedy rule is quoted" TO TK-NAME
           MOVE "LPI0006" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-LI-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "an alternative remedy displaces compensation"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE LI-COMPENSATION TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AR-024 - there must be an outstanding principal to charge
       1400-NOTHING-TO-CHARGE.
           PERFORM 0200-BUILD-CLAIM
           MOVE 0 TO LI-PRINCIPAL
           CALL "LATEINT" USING WS-LI-CA
           MOVE "a settled invoice carries no interest" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-LI-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-LI-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the no principal rule is quoted" TO TK-NAME
           MOVE "LPI0003" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-LI-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   D-28 - a waiver requires a reason
       1500-WAIVER-NEEDS-A-REASON.
           PERFORM 0200-BUILD-CLAIM
           MOVE 5000.00 TO LI-PRINCIPAL
           MOVE "ZZLP-WAIVE-0001" TO LI-CLAIM-REF
           MOVE "WAIVE" TO CA-FUNCTION OF WS-LI-CA
           CALL "LATEINT" USING WS-LI-CA
           MOVE "a waiver without a reason is blocked" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-LI-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-LI-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the waiver reason rule is quoted" TO TK-NAME
           MOVE "LPI0002" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-LI-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AR-024 - the reference rate is the base rate on 31 December
      *>   for interest arising 1 January to 30 June, or on 30 June for
      *>   1 July to 31 December
       1600-REFERENCE-RATE-BASIS.
           PERFORM 0200-BUILD-CLAIM
           MOVE 5000.00 TO LI-PRINCIPAL
           MOVE 0 TO LI-REF-RATE-PCT
           MOVE 20270315 TO LI-CALC-TO-DATE
           CALL "LATEINT" USING WS-LI-CA
           MOVE "a March 2027 claim uses the 31/12/2026 rate"
               TO TK-NAME
           MOVE 20261231 TO TK-EXPECTED-N
           MOVE LI-RATE-BASIS-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the statutory 8% is added to the base rate"
               TO TK-NAME
           MOVE 8.00 TO TK-EXPECTED-N
           COMPUTE WS-SPREAD = LI-TOTAL-RATE-PCT - LI-REF-RATE-PCT
           MOVE WS-SPREAD TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0200-BUILD-CLAIM
           MOVE 5000.00 TO LI-PRINCIPAL
           MOVE 0 TO LI-REF-RATE-PCT
           MOVE 20261015 TO LI-CALC-TO-DATE
           CALL "LATEINT" USING WS-LI-CA
           MOVE "an October 2026 claim uses the 30/06/2026 rate"
               TO TK-NAME
           MOVE 20260630 TO TK-EXPECTED-N
           MOVE LI-RATE-BASIS-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the rate source is recorded" TO TK-NAME
           MOVE "BOE base rate" TO TK-EXPECTED-X
           MOVE LI-RATE-SOURCE(1:13) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0200-BUILD-CLAIM
           MOVE 5000.00 TO LI-PRINCIPAL
           MOVE 25.00 TO LI-RECOVERY-COSTS
           CALL "LATEINT" USING WS-LI-CA
           MOVE "evidenced recovery costs join the claim" TO TK-NAME
           MOVE 144.32 TO TK-EXPECTED-N
           MOVE LI-TOTAL-CLAIM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AR-024 - a claim must point at a real receivable
       1700-UNKNOWN-INVOICE.
           PERFORM 0200-BUILD-CLAIM
           MOVE 5000.00 TO LI-PRINCIPAL
           MOVE "ZZLP-NOT-A-REAL-INVOICE" TO LI-INVOICE-NO
           CALL "LATEINT" USING WS-LI-CA
           MOVE "an unknown invoice is not found" TO TK-NAME
           MOVE 16 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-LI-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-LI-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the open item rule is quoted" TO TK-NAME
           MOVE "LPI0004" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-LI-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AC-121 end to end against the open item ledger, inside a
      *>   transaction that is rolled back
       1800-CLAIM-FROM-THE-LEDGER.
           PERFORM 1810-BEGIN-WORK
           PERFORM 1820-INSERT-FIXTURE
           PERFORM 0200-BUILD-CLAIM
           MOVE 0 TO LI-DAYS-OVERDUE
           MOVE "ZZLP-INV-0001" TO LI-INVOICE-NO
           MOVE "CALC" TO CA-FUNCTION OF WS-LI-CA
           CALL "LATEINT" USING WS-LI-CA
           MOVE "the principal comes from the ledger" TO TK-NAME
           MOVE 5000.00 TO TK-EXPECTED-N
           MOVE LI-PRINCIPAL TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-LI-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "15/09/2026 due, 15/10/2026 is 30 days late"
               TO TK-NAME
           MOVE 30 TO TK-EXPECTED-N
           MOVE LI-DAYS-OVERDUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "interest on the real receivable is 49.32" TO TK-NAME
           MOVE 49.32 TO TK-EXPECTED-N
           MOVE LI-INTEREST TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "compensation on the real receivable is 70.00"
               TO TK-NAME
           MOVE 70.00 TO TK-EXPECTED-N
           MOVE LI-COMPENSATION TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 1830-RECORD-THE-CLAIM
           PERFORM 1840-APPROVE-AND-WAIVE
           PERFORM 1890-ROLL-BACK.

       1810-BEGIN-WORK.
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

       1890-ROLL-BACK.
           MOVE "ROLB" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

       1820-INSERT-FIXTURE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT ar.OpenItem (CompanyCode, "
                  "CustomerCode, ItemType, DocumentNumber, "
                  "DocumentDate, DueDate, PostingDate, CurrencyCode, "
                  "FxRate, OriginalAmount, OriginalAmountGBP, "
                  "ResidualAmount, ResidualAmountGBP, VatAmountGBP, "
                  "SettlementStatus, CreatedBy) VALUES ('STC', "
                  "'C0004', 'INVOICE', 'ZZLP-INV-0001', "
                  "'2026-08-16', '2026-09-15', '2026-08-16', 'GBP', "
                  "1, 5000.00, 5000.00, 5000.00, 5000.00, 0, "
                  "'OPEN', 'ZZARTEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

      *>   D-28 - the claim is calculated automatically but issued
      *>   only on credit manager approval
       1830-RECORD-THE-CLAIM.
           MOVE "ZZLP-CLAIM-0001" TO LI-CLAIM-REF
           MOVE "CLAIM" TO CA-FUNCTION OF WS-LI-CA
           CALL "LATEINT" USING WS-LI-CA
           MOVE "a recorded claim awaits approval" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-LI-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-LI-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the approval rule is quoted" TO TK-NAME
           MOVE "LPI0008" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-LI-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the claim is held as calculated" TO TK-NAME
           MOVE "CALC" TO TK-EXPECTED-X
           MOVE LI-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the claim has its own reference" TO TK-NAME
           IF LI-CLAIM-ID > 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           PERFORM 1835-READ-CLAIM
           MOVE "the stored claim totals 119.32" TO TK-NAME
           MOVE 119.32 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-R1)
           PERFORM TK-ASSERT-NUM
           MOVE "the claim is linked to the original invoice"
               TO TK-NAME
           MOVE "ZZLP-INV-0001" TO TK-EXPECTED-X
           MOVE WS-R2 TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       1835-READ-CLAIM.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), TotalClaimGBP), "
                  "InvoiceNumber FROM ar.LatePaymentClaim "
                  "WHERE CompanyCode = 'STC' AND ClaimRef = "
                  "'ZZLP-CLAIM-0001'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           MOVE SPACES TO WS-ROW
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

      *>   D-28 - approval by the credit manager, then an authorised
      *>   waiver carrying its reason
       1840-APPROVE-AND-WAIVE.
           MOVE "CREDMGR" TO CA-USER OF WS-LI-CA
           MOVE "APPROVE" TO CA-FUNCTION OF WS-LI-CA
           CALL "LATEINT" USING WS-LI-CA
           MOVE "the credit manager approves the claim" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-LI-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-LI-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the claim becomes approved" TO TK-NAME
           MOVE "APPROVED" TO TK-EXPECTED-X
           MOVE LI-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "ZZLP-NO-SUCH-CLAIM" TO LI-CLAIM-REF
           MOVE "APPROVE" TO CA-FUNCTION OF WS-LI-CA
           CALL "LATEINT" USING WS-LI-CA
           MOVE "an unknown claim cannot be approved" TO TK-NAME
           MOVE 16 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-LI-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-LI-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "ZZLP-CLAIM-0001" TO LI-CLAIM-REF
           MOVE "Goodwill - long standing customer"
               TO LI-WAIVER-REASON
           MOVE "WAIVE" TO CA-FUNCTION OF WS-LI-CA
           CALL "LATEINT" USING WS-LI-CA
           MOVE "a reasoned waiver is accepted" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-LI-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-LI-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the claim is waived" TO TK-NAME
           MOVE "WAIVED" TO TK-EXPECTED-X
           MOVE LI-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       0900-SKIP-LEDGER-TESTS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 14
               MOVE "database not reachable - STERP_ODBC / SQL Server"
                   TO TK-DETAIL
               MOVE "TARLATE ledger fixture" TO TK-NAME
               PERFORM TK-SKIP
           END-PERFORM.

       COPY TESTKITP.
