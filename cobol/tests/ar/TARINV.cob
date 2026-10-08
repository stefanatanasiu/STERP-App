      *>================================================================
      *> PROGRAM-ID : TARINV
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR) UNIT TESTS
      *> PURPOSE    : ARINVPST - VAT INVOICE CONTENT, TERMS AND THE
      *>              POSTING OF ONE RECEIVABLE.  USES THE WORKED
      *>              NUMBERS OF ACCEPTANCE SCENARIO AC-120 AND THE
      *>              DEBTORS SPECIFICATION SECTION 10.
      *>              WRITES ARE CLEANED UP AT THE END, AND EVERY
      *>              FIXTURE CARRIES THE CORRELATION PREFIX ZZARTEST
      *>              SO THE SUITE IS REPEATABLE.
      *>----------------------------------------------------------------
      *> 2026-10-05 STERP ORIGINAL
      *>================================================================
      *> REQ: AR-003, AR-004, AR-005, AR-023   AC: AC-120, AC-019
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TARINV.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY ARINVPSTCA REPLACING ==DFHCOMMAREA== BY ==WS-INV-CA==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "Y".
       01  WS-PROBE.
           05  WS-PROBE-COL            PIC X(12).
           05  WS-PROBE-2              PIC X(12).
       01  WS-ROW.
           05  WS-R1                   PIC X(30).
           05  WS-R2                   PIC X(30).
           05  WS-R3                   PIC X(30).
           05  WS-R4                   PIC X(30).
       01  WS-MONEY-ED                 PIC -(13)9.99.
       01  WS-I                        PIC S9(4) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TARINV sales invoice posting" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-EVERYTHING
           ELSE
               PERFORM 0150-CLEAN-UP
               PERFORM 1000-FULL-VAT-INVOICE
               PERFORM 1100-DUE-DATE-FROM-TERMS
               PERFORM 1200-SIMPLIFIED-REFUSED
               PERFORM 1300-MISSING-ADDRESS
               PERFORM 1400-FOREIGN-CURRENCY
               PERFORM 1500-POST-AND-RECEIVABLE
               PERFORM 1600-REPEAT-IS-NOT-A-SECOND-BILL
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
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "N" TO WS-DB-UP
           END-IF.

      *>   remove anything an earlier run of this suite left behind
       0150-CLEAN-UP.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; DECLARE @i TABLE (Id bigint, "
                  "Num varchar(30)); INSERT @i SELECT InvoiceId, "
                  "InvoiceNumber FROM ar.SalesInvoice WHERE "
                  "CorrelationId LIKE 'ZZARTEST%'; "
                  "DELETE ar.OpenItem WHERE DocumentNumber IN "
                  "(SELECT Num FROM @i); "
                  "DELETE ar.SalesInvoiceVat WHERE InvoiceId IN "
                  "(SELECT Id FROM @i); "
                  "DELETE ar.SalesInvoiceLine WHERE InvoiceId IN "
                  "(SELECT Id FROM @i); "
                  "DELETE fin.VatTransaction WHERE DocumentRef IN "
                  "(SELECT Num FROM @i); "
                  "DELETE ar.SalesInvoice WHERE CorrelationId LIKE "
                  "'ZZARTEST%'; "
                  "DELETE fin.JournalLine WHERE JournalId IN "
                  "(SELECT JournalId FROM fin.Journal WHERE "
                  "CorrelationId LIKE 'ZZARTEST%'); "
                  "DELETE fin.Journal WHERE CorrelationId LIKE "
                  "'ZZARTEST%'; "
                  "DELETE fin.JournalStage WHERE CorrelationId LIKE "
                  "'ZZARTEST%'; "
                  "DELETE core.IdempotencyKey WHERE CorrelationId "
                  "LIKE 'ZZARTEST%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

      *>   a standard one line invoice ready to be altered per test
       0200-BUILD-STANDARD-INVOICE.
           INITIALIZE WS-INV-CA
           MOVE "STC" TO CA-COMPANY OF WS-INV-CA
           MOVE "RDG" TO CA-SITE OF WS-INV-CA
           MOVE "ARCLERK" TO CA-USER OF WS-INV-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-INV-CA
           MOVE "INVOICE" TO AI-DOC-TYPE
           MOVE "C0004" TO AI-CUSTOMER
           MOVE 20261015 TO AI-DOC-DATE
           MOVE 20261015 TO AI-TAX-POINT
           MOVE 20261015 TO AI-POSTING-DATE
           MOVE "GBP" TO AI-CURRENCY
           MOVE 1 TO AI-FX-RATE
           MOVE "DIRECT" TO AI-SOURCE
           MOVE "PO-77412" TO AI-ORDER-REF
           MOVE "Service charge agreement" TO AI-EVIDENCE-REF
           MOVE 1 TO AI-LINE-CNT
           MOVE "Annual frame assembly service" TO AI-L-DESC(1)
           MOVE 1 TO AI-L-QTY(1)
           MOVE 1000 TO AI-L-PRICE(1)
           MOVE "S" TO AI-L-VAT-CODE(1)
           MOVE "SALES" TO AI-L-COST-CENTRE(1).

      *>   AC-120 - a full VAT invoice of 1,000.00 plus 20% VAT
       1000-FULL-VAT-INVOICE.
           PERFORM 0200-BUILD-STANDARD-INVOICE
           MOVE "VALIDATE" TO CA-FUNCTION OF WS-INV-CA
           CALL "ARINVPST" USING WS-INV-CA
           MOVE AI-L-NET(1) TO WS-MONEY-ED
           MOVE AI-NET TO WS-MONEY-ED
           MOVE AI-L-PRICE(1) TO WS-MONEY-ED
           MOVE AI-L-QTY(1) TO WS-MONEY-ED
           MOVE "full VAT invoice validates" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-INV-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-INV-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "invoice net is 1000.00" TO TK-NAME
           MOVE 1000.00 TO TK-EXPECTED-N
           MOVE AI-NET TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "output VAT at 20% is 200.00" TO TK-NAME
           MOVE 200.00 TO TK-EXPECTED-N
           MOVE AI-VAT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "gross payable is 1200.00" TO TK-NAME
           MOVE 1200.00 TO TK-EXPECTED-N
           MOVE AI-GROSS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "one VAT analysis line" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE AI-VAT-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AR-003 - thirty day terms from 15/10/2026
       1100-DUE-DATE-FROM-TERMS.
           PERFORM 0200-BUILD-STANDARD-INVOICE
           MOVE "VALIDATE" TO CA-FUNCTION OF WS-INV-CA
           CALL "ARINVPST" USING WS-INV-CA
           MOVE "N30 due date is 14/11/2026" TO TK-NAME
           MOVE 20261114 TO TK-EXPECTED-N
           MOVE AI-DUE-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "terms are snapshotted on the document" TO TK-NAME
           IF AI-TERMS-SNAPSHOT NOT = SPACES
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>   AC-120 - a simplified invoice for 260.00 including VAT is
      *>   refused; the limit is 250.00
       1200-SIMPLIFIED-REFUSED.
           PERFORM 0200-BUILD-STANDARD-INVOICE
           MOVE "Y" TO AI-SIMPLIFIED
           MOVE 216.67 TO AI-L-PRICE(1)
           MOVE "VALIDATE" TO CA-FUNCTION OF WS-INV-CA
           CALL "ARINVPST" USING WS-INV-CA
           MOVE "260.00 simplified invoice is refused" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-INV-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-INV-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "refusal quotes the simplified invoice rule"
               TO TK-NAME
           MOVE "ARI0031" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-INV-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "gross of the refused document is 260.00" TO TK-NAME
           MOVE 260.00 TO TK-EXPECTED-N
           MOVE AI-GROSS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AC-120 - an invoice without the customer address is blocked
       1300-MISSING-ADDRESS.
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.CustomerAddress SET Line1 = '', "
                  "Town = NULL, Postcode = NULL WHERE "
                  "CompanyCode = 'STC' AND CustomerCode = 'C0004' "
                  "AND AddressRole = 'BILLING'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           PERFORM 0200-BUILD-STANDARD-INVOICE
           MOVE "VALIDATE" TO CA-FUNCTION OF WS-INV-CA
           CALL "ARINVPST" USING WS-INV-CA
           MOVE "invoice without a customer address is blocked"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-INV-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-INV-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the missing VAT element is named" TO TK-NAME
           MOVE "Customer address" TO TK-EXPECTED-X
           MOVE AI-MISSING(1) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "ROLB" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

      *>   AC-120 - a US dollar invoice shows the VAT total in
      *>   sterling with the rate used
       1400-FOREIGN-CURRENCY.
           PERFORM 0200-BUILD-STANDARD-INVOICE
           MOVE "C0027" TO AI-CUSTOMER
           MOVE "USD" TO AI-CURRENCY
           MOVE 1.27 TO AI-FX-RATE
           MOVE "Z" TO AI-L-VAT-CODE(1)
           MOVE "VALIDATE" TO CA-FUNCTION OF WS-INV-CA
           CALL "ARINVPST" USING WS-INV-CA
           MOVE "export invoice validates" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-INV-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-INV-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "1,000 USD at 1.27 is 787.40 sterling" TO TK-NAME
           MOVE 787.40 TO TK-EXPECTED-N
           MOVE AI-NET-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "zero rated export carries no VAT" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE AI-VAT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the rate used is stated on the document" TO TK-NAME
           IF AI-SPECIAL-WORDING(1:3) = "VAT"
               PERFORM TK-PASS
           ELSE
               MOVE AI-SPECIAL-WORDING TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>   AR-004 - one invoice creates exactly one receivable
       1500-POST-AND-RECEIVABLE.
           PERFORM 0200-BUILD-STANDARD-INVOICE
           MOVE "ZZARTEST-INV-0001" TO CA-CORRELATION-ID OF WS-INV-CA
           MOVE "POST" TO CA-FUNCTION OF WS-INV-CA
           CALL "ARINVPST" USING WS-INV-CA
           MOVE "the invoice posts" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-INV-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-INV-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "a journal number comes back" TO TK-NAME
           IF AI-JOURNAL-NO NOT = SPACES
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           PERFORM 1550-READ-OPEN-ITEM
           MOVE "exactly one receivable of 1200.00" TO TK-NAME
           MOVE 1200.00 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-R1)
           PERFORM TK-ASSERT-NUM
           MOVE "the receivable is open" TO TK-NAME
           MOVE "OPEN" TO TK-EXPECTED-X
           MOVE WS-R2 TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "one open item row, not two" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-R3)
           PERFORM TK-ASSERT-NUM
           MOVE "output VAT of 200.00 is in the VAT records"
               TO TK-NAME
           MOVE 200.00 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-R4)
           PERFORM TK-ASSERT-NUM.

       1550-READ-OPEN-ITEM.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), "
                  "ISNULL(SUM(o.ResidualAmountGBP),0)), "
                  "ISNULL(MAX(o.SettlementStatus),''), "
                  "CONVERT(varchar(24), COUNT(*)), "
                  "CONVERT(varchar(24), ISNULL((SELECT "
                  "SUM(v.VatAmountGBP) FROM fin.VatTransaction v "
                  "WHERE v.DocumentRef = ?),0)) "
                  "FROM ar.OpenItem o WHERE o.CompanyCode = 'STC' "
                  "AND o.DocumentNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AI-INVOICE-NO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE AI-INVOICE-NO TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 30 TO SQLIO-COL-WIDTH(3)
           MOVE 30 TO SQLIO-COL-WIDTH(4)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

      *>   COM-015 / AR-004 - repeating the instruction identifies the
      *>   original invoice instead of billing the work twice
       1600-REPEAT-IS-NOT-A-SECOND-BILL.
           MOVE AI-INVOICE-NO TO TK-EXPECTED-X
           PERFORM 0200-BUILD-STANDARD-INVOICE
           MOVE "ZZARTEST-INV-0001" TO CA-CORRELATION-ID OF WS-INV-CA
           MOVE "POST" TO CA-FUNCTION OF WS-INV-CA
           CALL "ARINVPST" USING WS-INV-CA
           MOVE "a repeated instruction warns, it does not re-bill"
               TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-INV-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-INV-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "it answers with the original invoice number"
               TO TK-NAME
           MOVE AI-INVOICE-NO TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 1550-READ-OPEN-ITEM
           MOVE "still exactly one receivable" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-R3)
           PERFORM TK-ASSERT-NUM.

       0900-SKIP-EVERYTHING.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 20
               MOVE "database not reachable - STERP_ODBC / SQL Server"
                   TO TK-DETAIL
               MOVE "TARINV sales invoice posting" TO TK-NAME
               PERFORM TK-SKIP
           END-PERFORM.

       COPY TESTKITP.
