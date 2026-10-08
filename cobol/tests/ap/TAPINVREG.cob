      *>================================================================
      *> PROGRAM-ID : TAPINVREG
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (AP) UNIT TESTS
      *> PURPOSE    : APINVREG - REGISTRATION, THE NORMALISED SUPPLIER
      *>              REFERENCE, EXACT DUPLICATE REFUSAL AND THE
      *>              SIMILAR DOCUMENT WARNING THAT A HUMAN MUST
      *>              DECIDE.  THE WORKED CASES ARE THOSE OF
      *>              ACCEPTANCE SCENARIO AC-029.
      *>              EVERY FIXTURE CARRIES THE CORRELATION PREFIX
      *>              ZZAPTEST AND IS REMOVED BEFORE AND AFTER THE
      *>              RUN, SO THE SUITE IS REPEATABLE.  WITH NO
      *>              DATABASE THE SUITE SKIPS RATHER THAN FAILS.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: AP-003, AP-004, AP-005, AP-023   AC: AC-029
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAPINVREG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY APINVREGCA REPLACING ==DFHCOMMAREA== BY ==WS-REG-CA==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "Y".
       01  WS-PROBE.
           05  WS-PROBE-1              PIC X(20).
           05  WS-PROBE-2              PIC X(20).
       01  WS-SAVED.
           05  WS-FIRST-REGISTER       PIC X(30) VALUE SPACES.
           05  WS-FIRST-NORMAL         PIC X(40) VALUE SPACES.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TAPINVREG register and dups" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-EVERYTHING
           ELSE
               PERFORM 0150-CLEAN-UP
               PERFORM 1000-VALIDATE-THE-NUMBERS
               PERFORM 1050-NONPO-CATEGORY-HELD
               PERFORM 1100-STATED-TOTALS-MUST-AGREE
               PERFORM 1200-RECEIVED-BEFORE-ISSUED
               PERFORM 1300-REGISTER-THE-GENUINE-ONE
               PERFORM 1400-CASE-AND-SPACE-IS-A-DUPLICATE
               PERFORM 1500-DUPLICATE-REGISTER-REFUSED
               PERFORM 1600-SIMILAR-IS-ONLY-A-WARNING
               PERFORM 1700-GENUINE-DOCUMENT-PROCEEDS
               PERFORM 0150-CLEAN-UP
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
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "N" TO WS-DB-UP
           END-IF.

       0900-SKIP-EVERYTHING.
           MOVE "purchase ledger registration" TO TK-NAME
           MOVE "ap.Supplier is not deployed or the server is down"
               TO TK-DETAIL
           PERFORM TK-SKIP.

      *>   remove anything an earlier run of this suite left behind
       0150-CLEAN-UP.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; DECLARE @i TABLE (Id bigint, "
                  "Num varchar(30)); INSERT @i SELECT "
                  "PurchaseInvoiceId, RegisterNumber FROM "
                  "ap.PurchaseInvoice WHERE CorrelationId LIKE "
                  "'ZZAPTEST%'; "
                  "DELETE ap.InvoiceMatch WHERE PurchaseInvoiceId "
                  "IN (SELECT Id FROM @i); "
                  "DELETE ap.OpenItem WHERE PurchaseInvoiceId IN "
                  "(SELECT Id FROM @i); "
                  "DELETE ap.PurchaseInvoiceVat WHERE "
                  "PurchaseInvoiceId IN (SELECT Id FROM @i); "
                  "DELETE ap.PurchaseInvoiceLine WHERE "
                  "PurchaseInvoiceId IN (SELECT Id FROM @i); "
                  "DELETE ap.PurchaseInvoice WHERE CorrelationId "
                  "LIKE 'ZZAPTEST%'; "
                  "DELETE core.IdempotencyKey WHERE CorrelationId "
                  "LIKE 'ZZAPTEST%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

      *>   a one line non order invoice: 10 at 25.00 plus 20 per cent
       0200-BUILD-INVOICE.
           INITIALIZE WS-REG-CA
           MOVE "STC" TO CA-COMPANY OF WS-REG-CA
           MOVE "RDG" TO CA-SITE OF WS-REG-CA
           MOVE "APCLERK" TO CA-USER OF WS-REG-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-REG-CA
           MOVE "ZZAPTEST-REG" TO CA-CORRELATION-ID OF WS-REG-CA
           MOVE "S0001" TO IR-SUPPLIER
           MOVE "INVOICE" TO IR-DOC-TYPE
           MOVE "ZZAPTEST/INV/77001" TO IR-SUPPLIER-REF
           MOVE 20261012 TO IR-DOCUMENT-DATE
           MOVE 20261015 TO IR-DATE-RECEIVED
           MOVE 20261012 TO IR-TAX-POINT
           MOVE 20261015 TO IR-POSTING-DATE
           MOVE "GBP" TO IR-CURRENCY
           MOVE 1 TO IR-FX-RATE
           MOVE "PAPER" TO IR-SOURCE-CHANNEL
           MOVE "GB123456789" TO IR-SUPPLIER-VATNO
           MOVE "VATINVOICE" TO IR-EVIDENCE-TYPE
           MOVE "Y" TO IR-IS-NON-PO
           MOVE "UTILITIES" TO IR-NONPO-REASON
           MOVE 250.00 TO IR-STATED-NET
           MOVE 50.00 TO IR-STATED-VAT
           MOVE 300.00 TO IR-STATED-GROSS
           MOVE 1 TO IR-LINE-CNT
           MOVE 1 TO IR-L-NUM(1)
           MOVE "Abrasive discs, 125mm" TO IR-L-DESC(1)
           MOVE 10 TO IR-L-QTY(1)
           MOVE "EA" TO IR-L-UOM(1)
           MOVE 1 TO IR-L-CONV(1)
           MOVE 25.00 TO IR-L-PRICE(1)
           MOVE 1 TO IR-L-PRICE-PER(1)
           MOVE "S" TO IR-L-VATCODE(1)
           MOVE "Y" TO IR-L-RECOVER(1)
           MOVE "6800" TO IR-L-ACCOUNT(1)
           MOVE "ADMIN" TO IR-L-COST-CTR(1).

      *>   AP-005 - the register works out the line extension, the VAT
      *>   and the sterling amounts from the coded detail
       1000-VALIDATE-THE-NUMBERS.
           PERFORM 0200-BUILD-INVOICE
           MOVE "VALIDATE" TO CA-FUNCTION OF WS-REG-CA
           CALL "APINVREG" USING WS-REG-CA
           MOVE "a coded invoice validates" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-REG-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-REG-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the calculated net is 250.00" TO TK-NAME
           MOVE 250.00 TO TK-EXPECTED-N
           MOVE IR-CALC-NET TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "input VAT at 20 per cent is 50.00" TO TK-NAME
           MOVE 50.00 TO TK-EXPECTED-N
           MOVE IR-CALC-VAT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the gross agrees with the supplier" TO TK-NAME
           MOVE 300.00 TO TK-EXPECTED-N
           MOVE IR-CALC-GROSS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "nothing is left to explain on the net" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE IR-NET-DIFF TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AP-005 - a line with no purchase order may only be coded
      *>   under an approved non-PO category; anything else is held
      *>   for an authorised exception rather than quietly accepted
       1050-NONPO-CATEGORY-HELD.
           PERFORM 0200-BUILD-INVOICE
           MOVE "WORKSHOP CONSUMABLES" TO IR-NONPO-REASON
           MOVE "VALIDATE" TO CA-FUNCTION OF WS-REG-CA
           CALL "APINVREG" USING WS-REG-CA
           MOVE "an unapproved non-PO category is held" TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-REG-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the non-PO exception is named" TO TK-NAME
           MOVE "APR0016" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-REG-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the held line is reported on the line" TO TK-NAME
           MOVE "HELD" TO TK-EXPECTED-X
           MOVE IR-L-STATUS(1) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AP-003 - the clerk keys what the supplier stated; a real
      *>   difference from the coded detail stops the document
       1100-STATED-TOTALS-MUST-AGREE.
           PERFORM 0200-BUILD-INVOICE
           MOVE 275.00 TO IR-STATED-NET
           MOVE 330.00 TO IR-STATED-GROSS
           MOVE "VALIDATE" TO CA-FUNCTION OF WS-REG-CA
           CALL "APINVREG" USING WS-REG-CA
           MOVE "a 25.00 difference is not accepted" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-REG-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-REG-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the difference is quantified" TO TK-NAME
           MOVE 25.00 TO TK-EXPECTED-N
           MOVE IR-NET-DIFF TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AP-003 - a document cannot be received before it was issued
       1200-RECEIVED-BEFORE-ISSUED.
           PERFORM 0200-BUILD-INVOICE
           MOVE 20261020 TO IR-DOCUMENT-DATE
           MOVE 20261015 TO IR-DATE-RECEIVED
           MOVE "VALIDATE" TO CA-FUNCTION OF WS-REG-CA
           CALL "APINVREG" USING WS-REG-CA
           MOVE "an impossible receipt date is refused" TO TK-NAME
           MOVE "APR0006" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-REG-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-REG-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

      *>   AC-029 - the genuine document registers.  Registration is
      *>   not posting and it is not approval to pay (AP-023).
       1300-REGISTER-THE-GENUINE-ONE.
           PERFORM 0200-BUILD-INVOICE
           MOVE "REGISTER" TO CA-FUNCTION OF WS-REG-CA
           CALL "APINVREG" USING WS-REG-CA
           MOVE "the genuine invoice is registered" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-REG-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-REG-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "registration completes without an error code"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-REG-CA = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-REG-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE IR-REGISTER-NUMBER TO WS-FIRST-REGISTER
           MOVE IR-REF-NORMALISED TO WS-FIRST-NORMAL
           MOVE "it is given a register number" TO TK-NAME
           IF IR-REGISTER-NUMBER NOT = SPACES
               MOVE IR-REGISTER-NUMBER TO TK-DETAIL
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           MOVE "it is not yet eligible for payment" TO TK-NAME
           IF IR-PAY-ELIGIBILITY = "PAYABLE"
               MOVE IR-PAY-ELIGIBILITY TO TK-DETAIL
               PERFORM TK-FAIL
           ELSE
               MOVE IR-PAY-ELIGIBILITY TO TK-DETAIL
               PERFORM TK-PASS
           END-IF.

      *>   AC-029 - the same reference keyed in lower case and with
      *>   outer spaces is the same document.  The comparison is made
      *>   on the normalised reference, never on what was typed.
       1400-CASE-AND-SPACE-IS-A-DUPLICATE.
           PERFORM 0200-BUILD-INVOICE
           MOVE "  zzaptest/inv/77001  " TO IR-SUPPLIER-REF
           MOVE "DUPCHK" TO CA-FUNCTION OF WS-REG-CA
           CALL "APINVREG" USING WS-REG-CA
           MOVE "case and spacing do not make a new document"
               TO TK-NAME
           MOVE WS-FIRST-NORMAL TO TK-EXPECTED-X
           MOVE IR-REF-NORMALISED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the earlier registration is found" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE IR-DUP-FOUND TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the clerk is pointed at the earlier entry"
               TO TK-NAME
           MOVE WS-FIRST-REGISTER TO TK-EXPECTED-X
           MOVE IR-DUP-REGISTER TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a duplicate is refused, not warned about"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-REG-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-REG-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the refusal quotes the duplicate rule" TO TK-NAME
           MOVE "APR0010" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-REG-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AC-029 - retrying the accepted batch must not create a
      *>   second liability
       1500-DUPLICATE-REGISTER-REFUSED.
           PERFORM 0200-BUILD-INVOICE
           MOVE "ZZAPTEST-INV-77001" TO IR-SUPPLIER-REF
           MOVE "REGISTER" TO CA-FUNCTION OF WS-REG-CA
           CALL "APINVREG" USING WS-REG-CA
           MOVE "registering it again is refused" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-REG-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-REG-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the attempt is marked as a duplicate" TO TK-NAME
           MOVE "DUPLICATE" TO TK-EXPECTED-X
           MOVE IR-REG-STATE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "no second register number is issued" TO TK-NAME
           MOVE WS-FIRST-REGISTER TO TK-EXPECTED-X
           MOVE IR-DUP-REGISTER TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AC-029 - the same amount and date under a different
      *>   reference is a warning for review, not proof of duplication
       1600-SIMILAR-IS-ONLY-A-WARNING.
           PERFORM 0200-BUILD-INVOICE
           MOVE "ZZAPTEST/INV/77002" TO IR-SUPPLIER-REF
           MOVE "DUPCHK" TO CA-FUNCTION OF WS-REG-CA
           CALL "APINVREG" USING WS-REG-CA
           MOVE "a different reference is not a duplicate" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE IR-DUP-FOUND TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the same amount and date are reported" TO TK-NAME
           IF IR-SIMILAR-CNT > 0
               MOVE IR-S-REASON(1) TO TK-DETAIL
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           MOVE "the similar document is only a warning" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-REG-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-REG-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the warning quotes the similar document rule"
               TO TK-NAME
           MOVE "APR0011" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-REG-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the earlier register number is offered" TO TK-NAME
           MOVE WS-FIRST-REGISTER TO TK-EXPECTED-X
           MOVE IR-S-REGISTER(1) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AC-029 - and the clerk may still accept it, because the
      *>   warning is a review and not an automatic rejection.  A new
      *>   correlation is used because the register number reserved
      *>   under a correlation is handed back unchanged on a retry.
       1700-GENUINE-DOCUMENT-PROCEEDS.
           PERFORM 0200-BUILD-INVOICE
           MOVE "ZZAPTEST-REG2" TO CA-CORRELATION-ID OF WS-REG-CA
           MOVE "ZZAPTEST/INV/77002" TO IR-SUPPLIER-REF
           MOVE "REGISTER" TO CA-FUNCTION OF WS-REG-CA
           CALL "APINVREG" USING WS-REG-CA
           MOVE "the reviewed document is registered" TO TK-NAME
           IF CA-RETURN-CODE OF WS-REG-CA = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-REG-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "it gets its own register number" TO TK-NAME
           IF IR-REGISTER-NUMBER NOT = WS-FIRST-REGISTER
              AND IR-REGISTER-NUMBER NOT = SPACES
               MOVE IR-REGISTER-NUMBER TO TK-DETAIL
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

       COPY TESTKITP.
