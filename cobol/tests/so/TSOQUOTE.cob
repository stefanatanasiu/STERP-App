      *>================================================================
      *> PROGRAM-ID : TSOQUOTE
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT UNIT TESTS
      *> PURPOSE    : SOQUOTE - THE QUOTATION LIFE CYCLE AND THE
      *>              CONVERSION OF AN ACCEPTED QUOTE INTO AN ORDER.
      *>              SO-001 NEEDS A QUOTE THAT IS NOT A COMMITMENT
      *>              UNTIL IT IS ACCEPTED ON RECORDED EVIDENCE, AND
      *>              A CONVERSION THAT CANNOT DEMAND TWICE.
      *>              SO-022 NEEDS VALIDITY TO BIND: AN EXPIRED
      *>              VERSION CANNOT BE ACCEPTED.
      *>              CR2601 (2026-10-07) NEEDS THE ORDER HEADER TO
      *>              BE COMPLETED FROM THE CUSTOMER MASTER AT
      *>              CONVERSION - INVOICED PARTY, DELIVERY ADDRESS,
      *>              PAYMENT TERMS, THE TRADING TERMS IN ISSUE AND
      *>              THE BASIS OF THE VAT TREATMENT - SO THAT THE
      *>              ORDER CAN ACTUALLY BE ACCEPTED.
      *>              FIXTURES: CUSTOMER ZZQCUST AND EVERYTHING IT
      *>              OWNS, REMOVED BEFORE AND AFTER THE RUN.
      *>----------------------------------------------------------------
      *> 2026-10-07 STERP ORIGINAL
      *>================================================================
      *> REQ: SO-001, SO-002, SO-011, SO-022, SO-024   AC: AC-039
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSOQUOTE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY SOQUOTECA REPLACING ==DFHCOMMAREA== BY ==WS-SQ-CA==.
       COPY SOACCEPTCA REPLACING ==DFHCOMMAREA== BY ==WS-AC-CA==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "Y".
           05  WS-HAVE-QUOTE           PIC X VALUE "N".
           05  WS-HAVE-ORDER           PIC X VALUE "N".
       01  WS-PROBE.
           05  WS-PROBE-1              PIC X(60).
           05  WS-PROBE-2              PIC X(60).
       01  WS-HDR.
           05  WS-H-INVCUST            PIC X(20).
           05  WS-H-DLVREF             PIC X(10).
           05  WS-H-DLVSNAP            PIC X(80).
           05  WS-H-TERMS              PIC X(12).
           05  WS-H-TCVER              PIC X(20).
           05  WS-H-VATBASIS           PIC X(20).
           05  WS-H-QUOTEREF           PIC X(30).
           05  WS-H-BILLSNAP           PIC X(80).
       01  WS-WORK.
           05  WS-QUOTE-1              PIC X(30) VALUE SPACES.
           05  WS-QUOTE-2              PIC X(30) VALUE SPACES.
           05  WS-ORDER-1              PIC X(30) VALUE SPACES.
           05  WS-ORDER-2              PIC X(30) VALUE SPACES.
           05  WS-TODAY                PIC 9(8) VALUE 0.
           05  WS-EXPIRED              PIC 9(8) VALUE 0.
           05  WS-COUNT                PIC S9(9) VALUE 0.
           05  WS-IX                   PIC S9(4) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TSOQUOTE quote to order" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-EVERYTHING
           ELSE
               PERFORM 0150-CLEAN-UP
               PERFORM 0160-MAKE-CUSTOMER
               PERFORM 1000-CREATE-IS-A-DRAFT
               PERFORM 1100-ISSUE-THE-VERSION
               PERFORM 1200-ACCEPT-NEEDS-EVIDENCE
               PERFORM 1300-ACCEPT-ON-EVIDENCE
               PERFORM 1400-CONVERT-MAKES-AN-ORDER
               PERFORM 1500-HEADER-COMES-FROM-MASTER
               PERFORM 1600-ORDER-CAN-BE-ACCEPTED
               PERFORM 1700-CONVERT-ONLY-ONCE
               PERFORM 1800-EXPIRED-CANNOT-BE-ACCEPTED
               PERFORM 1900-CALLER-VALUES-ARE-KEPT
               PERFORM 0150-CLEAN-UP
           END-IF
           PERFORM TK-END
           STOP RUN.

       0100-PROBE-DATABASE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CompanyCode, '' FROM core.Company "
                  "WHERE CompanyCode = 'STC'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 60 TO SQLIO-COL-WIDTH(1)
           MOVE 60 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "N" TO WS-DB-UP
           END-IF
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           COMPUTE WS-EXPIRED = FUNCTION INTEGER-OF-DATE(WS-TODAY)
           COMPUTE WS-EXPIRED = FUNCTION DATE-OF-INTEGER(WS-EXPIRED
                   - 10).

      *>   everything the fixture customer owns, in dependency order
       0150-CLEAN-UP.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; DELETE so.DeliverySchedule WHERE "
                  "SalesOrderLineId IN (SELECT l.SalesOrderLineId "
                  "FROM so.SalesOrderLine l JOIN so.SalesOrder o ON "
                  "o.SalesOrderId = l.SalesOrderId WHERE "
                  "o.CustomerCode = 'ZZQCUST'); DELETE "
                  "so.SalesOrderLine WHERE SalesOrderId IN (SELECT "
                  "SalesOrderId FROM so.SalesOrder WHERE "
                  "CustomerCode = 'ZZQCUST'); DELETE so.SalesOrder "
                  "WHERE CustomerCode = 'ZZQCUST'; DELETE "
                  "so.QuoteLine WHERE QuoteId IN (SELECT QuoteId "
                  "FROM so.Quote WHERE CustomerCode = 'ZZQCUST'); "
                  "DELETE so.Quote WHERE CustomerCode = 'ZZQCUST'; "
                  "DELETE core.IdempotencyKey WHERE CorrelationId "
                  "LIKE 'ZZSOQT%'; DELETE ar.CustomerAddress WHERE "
                  "CustomerCode = 'ZZQCUST'; DELETE "
                  "ar.CustomerCredit WHERE CustomerCode = 'ZZQCUST';"
                  " DELETE ar.Customer WHERE CustomerCode = "
                  "'ZZQCUST'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-RUN-STATEMENT.

      *>   the account the quote is priced for.  Its master data is
      *>   the whole point of the conversion test: the order header
      *>   has to come from here, not from the quotation.
       0160-MAKE-CUSTOMER.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT INTO ar.Customer "
                  "(CompanyCode, CustomerCode, LegalName, LegalForm,"
                  " VatLocation, VatRegNo, IsVatBusiness, "
                  "CountryCode, CurrencyCode, AllowedCurrencies, "
                  "TermsCode, CollectionMethod, StatementFrequency, "
                  "SalespersonCode, Classification, SanctionsStatus,"
                  " Status, CreatedBy) VALUES ('STC', 'ZZQCUST', "
                  "'ZZ quotation fixture account', 'LTD', 'GB', "
                  "'GB999111222', 1, 'GB', 'GBP', 'GBP', 'N30', "
                  "'BACS', 'MONTHLY', 'SALESREP', 'TEST', 'CLEAR', "
                  "'ACTIVE', 'TSOQUOTE'); INSERT INTO "
                  "ar.CustomerAddress (CompanyCode, CustomerCode, "
                  "AddressRole, AddressRef, Name, Line1, Town, "
                  "Postcode, CountryCode, IsDefault, CreatedBy) "
                  "VALUES ('STC','ZZQCUST','DELIVERY','MAIN','ZZ "
                  "quotation fixture account','Unit 9, Kennet Trade "
                  "Park','Reading','RG1 1AA','GB',1,'TSOQUOTE'); "
                  "INSERT INTO ar.CustomerAddress (CompanyCode, "
                  "CustomerCode, AddressRole, AddressRef, Name, "
                  "Line1, Town, Postcode, CountryCode, IsDefault, "
                  "CreatedBy) VALUES ('STC','ZZQCUST','BILLING',"
                  "'MAIN','ZZ quotation fixture account','Accounts "
                  "Payable, PO Box 44','Reading','RG1 1AB','GB',1,"
                  "'TSOQUOTE'); INSERT INTO ar.CustomerCredit "
                  "(CompanyCode, CustomerCode, CreditLimitGBP, "
                  "OverdueThresholdGBP, OverdueDaysTrigger, "
                  "RiskClass, ApprovedBy, ApprovedAt, HoldFlag, "
                  "CreatedBy) VALUES ('STC','ZZQCUST',250000.00,"
                  "25000.00,30,'LOW','CREDITCTL',SYSDATETIME(),0,"
                  "'TSOQUOTE')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-RUN-STATEMENT.

      *>----------------------------------------------------------------
      *>   SO-001.  A quotation is a priced offer, not demand: it is
      *>   born DRAFT and it is numbered from the quotation series.
       1000-CREATE-IS-A-DRAFT.
           PERFORM 0200-RESET-QUOTE-CA
           MOVE "CREATE" TO CA-FUNCTION OF WS-SQ-CA
           MOVE "ZZSOQT-CREATE-1" TO CA-CORRELATION-ID OF WS-SQ-CA
           MOVE "ZZQCUST" TO SQ-CUSTOMER
           MOVE WS-TODAY TO SQ-QUOTE-DATE
           MOVE 30 TO SQ-VALID-DAYS
           PERFORM 0250-ONE-LINE
           CALL "SOQUOTE" USING WS-SQ-CA
           MOVE "CREATE numbers the quote and leaves it DRAFT"
               TO TK-NAME
           MOVE CA-MESSAGE OF WS-SQ-CA TO TK-DETAIL
           IF CA-RETURN-CODE OF WS-SQ-CA < 4
              AND SQ-QUOTE-NUMBER NOT = SPACES
              AND SQ-STATUS = "DRAFT"
               MOVE SQ-QUOTE-NUMBER TO WS-QUOTE-1
               MOVE "Y" TO WS-HAVE-QUOTE
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

       1100-ISSUE-THE-VERSION.
           MOVE "ISSUE sends version 1 to the customer" TO TK-NAME
           IF WS-HAVE-QUOTE NOT = "Y"
               MOVE "no quote was created" TO TK-DETAIL
               PERFORM TK-SKIP
           ELSE
               PERFORM 0200-RESET-QUOTE-CA
               MOVE "ISSUE" TO CA-FUNCTION OF WS-SQ-CA
               MOVE "ZZSOQT-ISSUE-1" TO CA-CORRELATION-ID OF WS-SQ-CA
               MOVE WS-QUOTE-1 TO SQ-QUOTE-NUMBER
               CALL "SOQUOTE" USING WS-SQ-CA
               MOVE CA-MESSAGE OF WS-SQ-CA TO TK-DETAIL
               MOVE "ISSUED" TO TK-EXPECTED-X
               MOVE SQ-STATUS TO TK-ACTUAL-X
               PERFORM TK-ASSERT-STR
           END-IF.

      *>   SO-001.  Without evidence of acceptance there is nothing
      *>   to defend the order with, so the instruction is refused.
       1200-ACCEPT-NEEDS-EVIDENCE.
           MOVE "ACCEPT without evidence is refused (SOQ0001)"
               TO TK-NAME
           IF WS-HAVE-QUOTE NOT = "Y"
               MOVE "no quote was created" TO TK-DETAIL
               PERFORM TK-SKIP
           ELSE
               PERFORM 0200-RESET-QUOTE-CA
               MOVE "ACCEPT" TO CA-FUNCTION OF WS-SQ-CA
               MOVE WS-QUOTE-1 TO SQ-QUOTE-NUMBER
               MOVE SPACES TO SQ-ACCEPT-EVIDENCE
               CALL "SOQUOTE" USING WS-SQ-CA
               MOVE CA-MESSAGE OF WS-SQ-CA TO TK-DETAIL
               IF CA-RETURN-CODE OF WS-SQ-CA >= 8
                  AND CA-REASON-CODE OF WS-SQ-CA = "SOQ0001"
                   PERFORM TK-PASS
               ELSE
                   PERFORM TK-FAIL
               END-IF
           END-IF.

       1300-ACCEPT-ON-EVIDENCE.
           MOVE "ACCEPT records how the customer accepted" TO TK-NAME
           IF WS-HAVE-QUOTE NOT = "Y"
               MOVE "no quote was created" TO TK-DETAIL
               PERFORM TK-SKIP
           ELSE
               PERFORM 0200-RESET-QUOTE-CA
               MOVE "ACCEPT" TO CA-FUNCTION OF WS-SQ-CA
               MOVE "ZZSOQT-ACCEPT-1" TO CA-CORRELATION-ID OF
                   WS-SQ-CA
               MOVE WS-QUOTE-1 TO SQ-QUOTE-NUMBER
               MOVE "Customer e-mail of 10:14, filed as ZZ/1"
                   TO SQ-ACCEPT-EVIDENCE
               CALL "SOQUOTE" USING WS-SQ-CA
               MOVE CA-MESSAGE OF WS-SQ-CA TO TK-DETAIL
               MOVE "ACCEPTED" TO TK-EXPECTED-X
               MOVE SQ-STATUS TO TK-ACTUAL-X
               PERFORM TK-ASSERT-STR
           END-IF.

      *>   SO-001.  The conversion keeps the provenance: the order
      *>   says which quotation it came from.
       1400-CONVERT-MAKES-AN-ORDER.
           MOVE "CONVERT raises an order from the accepted quote"
               TO TK-NAME
           IF WS-HAVE-QUOTE NOT = "Y"
               MOVE "no quote was created" TO TK-DETAIL
               PERFORM TK-SKIP
           ELSE
               PERFORM 0200-RESET-QUOTE-CA
               MOVE "CONVERT" TO CA-FUNCTION OF WS-SQ-CA
               MOVE "ZZSOQT-CONVERT-1" TO CA-CORRELATION-ID OF
                   WS-SQ-CA
               MOVE WS-QUOTE-1 TO SQ-QUOTE-NUMBER
               MOVE "ZZSOQT-REF-1" TO SQ-CUSTOMER-REF
               MOVE "STANDARD" TO SQ-ORDER-TYPE
               MOVE "RDG" TO SQ-SITE-CODE
               CALL "SOQUOTE" USING WS-SQ-CA
               MOVE CA-MESSAGE OF WS-SQ-CA TO TK-DETAIL
               IF CA-RETURN-CODE OF WS-SQ-CA < 8
                  AND SQ-ORDER-NUMBER NOT = SPACES
                   MOVE SQ-ORDER-NUMBER TO WS-ORDER-1
                   MOVE "Y" TO WS-HAVE-ORDER
                   PERFORM TK-PASS
               ELSE
                   PERFORM TK-FAIL
               END-IF
           END-IF.

      *>----------------------------------------------------------------
      *>   CR2601.  The five things an order needs before it can bind
      *>   and that a quotation never states.  Each one is asserted
      *>   against what was actually written to so.SalesOrder.
       1500-HEADER-COMES-FROM-MASTER.
           IF WS-HAVE-ORDER NOT = "Y"
               MOVE "CONVERT did not produce an order" TO TK-DETAIL
               MOVE "the order header is taken from the customer"
                   TO TK-NAME
               PERFORM TK-SKIP
           ELSE
               PERFORM 0300-READ-ORDER-HEADER
               MOVE "CONVERT defaults the invoiced party" TO TK-NAME
               MOVE "ZZQCUST" TO TK-EXPECTED-X
               MOVE WS-H-INVCUST TO TK-ACTUAL-X
               PERFORM TK-ASSERT-STR
               MOVE "CONVERT defaults the delivery address"
                   TO TK-NAME
               MOVE WS-H-DLVSNAP TO TK-DETAIL
               IF WS-H-DLVSNAP(1:14) = "ZZ quotation f"
                  AND WS-H-DLVREF = "MAIN"
                   PERFORM TK-PASS
               ELSE
                   PERFORM TK-FAIL
               END-IF
               MOVE "CONVERT defaults the payment terms" TO TK-NAME
               MOVE "N30" TO TK-EXPECTED-X
               MOVE WS-H-TERMS TO TK-ACTUAL-X
               PERFORM TK-ASSERT-STR
               MOVE "CONVERT records the trading terms in issue"
                   TO TK-NAME
               MOVE WS-H-TCVER TO TK-DETAIL
               IF WS-H-TCVER NOT = SPACES
                   PERFORM TK-PASS
               ELSE
                   PERFORM TK-FAIL
               END-IF
               MOVE "CONVERT states the VAT treatment basis"
                   TO TK-NAME
               MOVE "UK standard rated" TO TK-EXPECTED-X
               MOVE WS-H-VATBASIS TO TK-ACTUAL-X
               PERFORM TK-ASSERT-STR
               MOVE "the order keeps the quotation provenance"
                   TO TK-NAME
               MOVE WS-QUOTE-1 TO TK-EXPECTED-X
               MOVE WS-H-QUOTEREF TO TK-ACTUAL-X
               PERFORM TK-ASSERT-STR
           END-IF.

      *>   AC-039 read the other way round: a complete header means
      *>   SOACCEPT finds nothing to block, so the order can bind.
       1600-ORDER-CAN-BE-ACCEPTED.
           MOVE "the converted order has no acceptance blockers"
               TO TK-NAME
           IF WS-HAVE-ORDER NOT = "Y"
               MOVE "CONVERT did not produce an order" TO TK-DETAIL
               PERFORM TK-SKIP
           ELSE
               INITIALIZE WS-AC-CA
               MOVE "SOACCEPT" TO CA-PROGRAM OF WS-AC-CA
               MOVE "VALIDATE" TO CA-FUNCTION OF WS-AC-CA
               MOVE "STC" TO CA-COMPANY OF WS-AC-CA
               MOVE "RDG" TO CA-SITE OF WS-AC-CA
               MOVE "SALESREP" TO CA-USER OF WS-AC-CA
               MOVE WS-TODAY TO CA-BUSINESS-DATE OF WS-AC-CA
               MOVE "ZZSOQT-VALIDATE-1" TO CA-CORRELATION-ID OF
                   WS-AC-CA
               MOVE WS-ORDER-1 TO SA-ORDER-NUMBER
               CALL "SOACCEPT" USING WS-AC-CA
               MOVE SPACES TO TK-DETAIL
               IF SA-BLOCKER-CNT > 0
                   MOVE SA-B-TEXT(1) TO TK-DETAIL
               END-IF
               MOVE 0 TO TK-EXPECTED-N
               MOVE SA-BLOCKER-CNT TO TK-ACTUAL-N
               PERFORM TK-ASSERT-NUM
           END-IF.

      *>   COM-015.  The same instruction twice is still one order.
       1700-CONVERT-ONLY-ONCE.
           MOVE "a repeated CONVERT returns the first order"
               TO TK-NAME
           IF WS-HAVE-ORDER NOT = "Y"
               MOVE "CONVERT did not produce an order" TO TK-DETAIL
               PERFORM TK-SKIP
           ELSE
               PERFORM 0200-RESET-QUOTE-CA
               MOVE "CONVERT" TO CA-FUNCTION OF WS-SQ-CA
               MOVE "ZZSOQT-CONVERT-1" TO CA-CORRELATION-ID OF
                   WS-SQ-CA
               MOVE WS-QUOTE-1 TO SQ-QUOTE-NUMBER
               MOVE "ZZSOQT-REF-1" TO SQ-CUSTOMER-REF
               MOVE "STANDARD" TO SQ-ORDER-TYPE
               MOVE "RDG" TO SQ-SITE-CODE
               CALL "SOQUOTE" USING WS-SQ-CA
               MOVE CA-MESSAGE OF WS-SQ-CA TO TK-DETAIL
               MOVE WS-ORDER-1 TO TK-EXPECTED-X
               MOVE SQ-ORDER-NUMBER TO TK-ACTUAL-X
               PERFORM TK-ASSERT-STR
               PERFORM 0350-COUNT-ORDERS
               MOVE "the second attempt made no second demand"
                   TO TK-NAME
               MOVE 1 TO TK-EXPECTED-N
               MOVE WS-COUNT TO TK-ACTUAL-N
               PERFORM TK-ASSERT-NUM
           END-IF.

      *>   SO-022.  Validity binds: an expired version is re-quoted,
      *>   never accepted.
       1800-EXPIRED-CANNOT-BE-ACCEPTED.
           PERFORM 0200-RESET-QUOTE-CA
           MOVE "CREATE" TO CA-FUNCTION OF WS-SQ-CA
           MOVE "ZZSOQT-CREATE-2" TO CA-CORRELATION-ID OF WS-SQ-CA
           MOVE "ZZQCUST" TO SQ-CUSTOMER
           MOVE WS-EXPIRED TO SQ-QUOTE-DATE
           MOVE WS-EXPIRED TO SQ-VALID-UNTIL
           PERFORM 0250-ONE-LINE
           CALL "SOQUOTE" USING WS-SQ-CA
           MOVE SQ-QUOTE-NUMBER TO WS-QUOTE-2
           MOVE "ISSUE" TO CA-FUNCTION OF WS-SQ-CA
           MOVE "ZZSOQT-ISSUE-2" TO CA-CORRELATION-ID OF WS-SQ-CA
           CALL "SOQUOTE" USING WS-SQ-CA
           MOVE "ACCEPT" TO CA-FUNCTION OF WS-SQ-CA
           MOVE "ZZSOQT-ACCEPT-2" TO CA-CORRELATION-ID OF WS-SQ-CA
           MOVE WS-QUOTE-2 TO SQ-QUOTE-NUMBER
           MOVE "Customer telephone call, no written record"
               TO SQ-ACCEPT-EVIDENCE
           CALL "SOQUOTE" USING WS-SQ-CA
           MOVE "an expired version cannot be accepted (SOQ0006)"
               TO TK-NAME
           MOVE CA-MESSAGE OF WS-SQ-CA TO TK-DETAIL
           IF CA-RETURN-CODE OF WS-SQ-CA >= 8
              AND CA-REASON-CODE OF WS-SQ-CA = "SOQ0006"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>   CR2601.  Defaulting is a default, not a policy: what the
      *>   order desk states is what is written.
       1900-CALLER-VALUES-ARE-KEPT.
           MOVE "a stated delivery address is not overwritten"
               TO TK-NAME
           IF WS-HAVE-QUOTE NOT = "Y"
               MOVE "no quote was created" TO TK-DETAIL
               PERFORM TK-SKIP
           ELSE
               PERFORM 0200-RESET-QUOTE-CA
               MOVE "TEMPLATE" TO CA-FUNCTION OF WS-SQ-CA
               MOVE "ZZSOQT-TEMPLATE-1" TO CA-CORRELATION-ID OF
                   WS-SQ-CA
               MOVE "QUOTE" TO SQ-SOURCE-TYPE
               MOVE WS-QUOTE-1 TO SQ-SOURCE-REF
               MOVE "ZZSOQT-REF-2" TO SQ-CUSTOMER-REF
               MOVE "RDG" TO SQ-SITE-CODE
               MOVE "Site store, Gate 4, Theale, RG7 4AB"
                   TO SQ-DELIV-SNAP
               CALL "SOQUOTE" USING WS-SQ-CA
               MOVE CA-MESSAGE OF WS-SQ-CA TO TK-DETAIL
               IF CA-RETURN-CODE OF WS-SQ-CA < 8
                  AND SQ-ORDER-NUMBER NOT = SPACES
                   MOVE SQ-ORDER-NUMBER TO WS-ORDER-2
                   MOVE WS-ORDER-2 TO WS-ORDER-1
                   PERFORM 0300-READ-ORDER-HEADER
                   MOVE WS-ORDER-2 TO WS-ORDER-1
                   MOVE "Site store, Gate 4, Theale, RG7 4AB"
                       TO TK-EXPECTED-X
                   MOVE WS-H-DLVSNAP TO TK-ACTUAL-X
                   PERFORM TK-ASSERT-STR
               ELSE
                   PERFORM TK-FAIL
               END-IF
           END-IF.

      *>----------------------------------------------------------------
       0200-RESET-QUOTE-CA.
           INITIALIZE WS-SQ-CA
           MOVE "SOQUOTE" TO CA-PROGRAM OF WS-SQ-CA
           MOVE "STC" TO CA-COMPANY OF WS-SQ-CA
           MOVE "RDG" TO CA-SITE OF WS-SQ-CA
           MOVE "SALESREP" TO CA-USER OF WS-SQ-CA
           MOVE WS-TODAY TO CA-BUSINESS-DATE OF WS-SQ-CA
           MOVE "GBP" TO SQ-CURRENCY
           MOVE 1 TO SQ-FX-RATE
           MOVE "RDG" TO SQ-SITE-CODE
           MOVE "SALESREP" TO SQ-SALESPERSON
           MOVE 0 TO SQ-LINE-CNT.

      *>   one priced line, on an item this company really sells
       0250-ONE-LINE.
           MOVE 1 TO SQ-LINE-CNT
           MOVE 10 TO SQ-L-LINE-NUM(1)
           MOVE "FG-FRAME-ROAD-SL-54" TO SQ-L-ITEM(1)
           MOVE "Frame, road Superlight, 54 cm" TO SQ-L-DESC(1)
           MOVE 4 TO SQ-L-QTY(1)
           MOVE "EA" TO SQ-L-UOM(1)
           MOVE 342 TO SQ-L-UNIT-PRICE(1)
           MOVE 1 TO SQ-L-PRICE-PER(1)
           MOVE "S" TO SQ-L-VAT-CODE(1)
           MOVE 1368 TO SQ-L-NET(1)
           MOVE 273.60 TO SQ-L-VAT-AMT(1)
           MOVE 944 TO SQ-L-COST-GBP(1)
           MOVE "STANDARD" TO SQ-L-COST-BASIS(1).

       0300-READ-ORDER-HEADER.
           INITIALIZE WS-HDR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(o.InvoiceCustomerCode,''), "
                  "ISNULL(o.DeliveryAddressRef,''), "
                  "ISNULL(o.DeliveryAddressSnapshot,''), "
                  "ISNULL(o.TermsCode,''), "
                  "ISNULL(o.TermsAndConditionsVersion,''), "
                  "ISNULL(o.VatTreatmentBasis,''), "
                  "ISNULL(o.QuoteNumber,''), "
                  "ISNULL(o.BillToAddressSnapshot,'') FROM "
                  "so.SalesOrder o WHERE o.CompanyCode = 'STC' AND "
                  "o.OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-ORDER-1 TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 8 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 10 TO SQLIO-COL-WIDTH(2)
           MOVE 80 TO SQLIO-COL-WIDTH(3)
           MOVE 12 TO SQLIO-COL-WIDTH(4)
           MOVE 20 TO SQLIO-COL-WIDTH(5)
           MOVE 20 TO SQLIO-COL-WIDTH(6)
           MOVE 30 TO SQLIO-COL-WIDTH(7)
           MOVE 80 TO SQLIO-COL-WIDTH(8)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-HDR.

       0350-COUNT-ORDERS.
           MOVE 0 TO WS-COUNT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12), COUNT(*)), '' FROM "
                  "so.SalesOrder WHERE CompanyCode = 'STC' AND "
                  "QuoteNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-QUOTE-1 TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 60 TO SQLIO-COL-WIDTH(1)
           MOVE 60 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-OK
               COMPUTE WS-COUNT = FUNCTION NUMVAL(WS-PROBE-1)
           END-IF.

       0700-RUN-STATEMENT.
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR
               DISPLAY "TSOQUOTE SQL: " SQLIO-ERRMSG UPON SYSERR
           END-IF.

       0900-SKIP-EVERYTHING.
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 14
               MOVE "SOQUOTE needs the STERP database" TO TK-NAME
               MOVE "database not reachable" TO TK-DETAIL
               PERFORM TK-SKIP
           END-PERFORM.

       COPY TESTKITP.
