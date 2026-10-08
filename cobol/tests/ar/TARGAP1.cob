      *>================================================================
      *> PROGRAM-ID : TARGAP1
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR) UNIT TESTS
      *> PURPOSE    : CUSTVAL AND ARCRNOTE - THE ACCOUNT LIFECYCLE,
      *>              THE SEPARATION OF THE ORDERING, INVOICED,
      *>              PAYING AND STATEMENT PARTIES, AND THE RULE
      *>              THAT A FINANCIAL CREDIT NEITHER EDITS THE
      *>              ORIGINAL INVOICE NOR RETURNS STOCK.
      *>              EVERY WRITE RUNS INSIDE BEGN ... ROLB SO THE
      *>              SUITE IS REPEATABLE AND LEAVES NOTHING BEHIND.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2026-10-07 STERP ORIGINAL - CLOSING THE AR TEST GAPS
      *>================================================================
      *> REQ: AR-001, AR-002, AR-006
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TARGAP1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY CUSTVALCA REPLACING ==DFHCOMMAREA== BY ==WS-CV-CA==.
       COPY ARCRNOTECA REPLACING ==DFHCOMMAREA== BY ==WS-CN-CA==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "N".
       01  WS-ROW.
           05  WS-R1                   PIC X(30).
           05  WS-R2                   PIC X(30).
       01  WS-WORK.
           05  WS-DEBT-BEFORE          PIC S9(13)V99 VALUE 0.
           05  WS-DEBT-AFTER           PIC S9(13)V99 VALUE 0.
           05  WS-ITEMS-BEFORE         PIC S9(9) VALUE 0.
           05  WS-ITEMS-AFTER          PIC S9(9) VALUE 0.
           05  WS-GROSS-BEFORE         PIC S9(13)V99 VALUE 0.
           05  WS-GROSS-AFTER          PIC S9(13)V99 VALUE 0.
           05  WS-EXPECT               PIC S9(13)V99 VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TARGAP1 customer lifecycle, parties, credit notes"
               TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP NOT = "Y"
               PERFORM 0900-SKIP-EVERYTHING
           ELSE
               PERFORM 1000-ACCOUNT-COMPLETENESS
               PERFORM 1100-STATUS-NEEDS-REASON
               PERFORM 1200-RETIRE-KEEPS-THE-DEBT
               PERFORM 2000-PARTY-ROLES
               PERFORM 2100-PAYER-DOES-NOT-POOL
               PERFORM 3000-CREDIT-RETURNS-NO-STOCK
               PERFORM 3100-CREDIT-NAMES-ITS-INVOICE
               PERFORM 3200-REMAINING-CREDITABLE
           END-IF
           PERFORM TK-END
           STOP RUN.

       0100-PROBE-DATABASE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CustomerCode, '' FROM ar.Customer "
                  "WHERE CompanyCode = 'STC' AND CustomerCode = "
                  "'C0001'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-SQLCODE = 0 AND WS-R1 NOT = SPACES
               MOVE "Y" TO WS-DB-UP
           END-IF.

       0900-SKIP-EVERYTHING.
           MOVE "no database or no seeded sales ledger" TO TK-DETAIL
           MOVE "AR-001 customer account lifecycle" TO TK-NAME
           PERFORM TK-SKIP
           MOVE "no database or no seeded sales ledger" TO TK-DETAIL
           MOVE "AR-002 ordering, paying and statement parties"
               TO TK-NAME
           PERFORM TK-SKIP
           MOVE "no database or no seeded sales ledger" TO TK-DETAIL
           MOVE "AR-006 credit note content and limits" TO TK-NAME
           PERFORM TK-SKIP.

       8000-BEGIN-TRAN.
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

       8100-ROLLBACK-TRAN.
           MOVE "ROLB" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

      *>   one scalar answer into WS-R1, statement already in
      *>   SQLIO-STMT; the caller converts with FUNCTION NUMVAL
       8200-ONE-VALUE.
           MOVE SPACES TO WS-R1 WS-R2
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

       8300-PRIME-CUSTVAL.
           INITIALIZE WS-CV-CA
           MOVE "STC" TO CA-COMPANY OF WS-CV-CA
           MOVE "RDG" TO CA-SITE OF WS-CV-CA
           MOVE "ARCLERK1" TO CA-USER OF WS-CV-CA
           MOVE 20261007 TO CA-BUSINESS-DATE OF WS-CV-CA
           MOVE "C0001" TO CV-CUSTOMER.

       8400-PRIME-CREDIT.
           INITIALIZE WS-CN-CA
           MOVE "STC" TO CA-COMPANY OF WS-CN-CA
           MOVE "RDG" TO CA-SITE OF WS-CN-CA
           MOVE "ARCLERK1" TO CA-USER OF WS-CN-CA
           MOVE 20261007 TO CA-BUSINESS-DATE OF WS-CN-CA
           MOVE "C0001" TO AN-CUSTOMER
           MOVE "VALUE" TO AN-CREDIT-TYPE
           MOVE "PRICE" TO AN-REASON-CODE
           MOVE "Agreed price adjustment" TO AN-REASON
           MOVE "GBP" TO AN-CURRENCY
           MOVE 1 TO AN-FX-RATE
           MOVE 20261007 TO AN-DOC-DATE
           MOVE 20261007 TO AN-TAX-POINT
           MOVE 20261007 TO AN-POSTING-DATE
           MOVE "N" TO AN-RETURN-STOCK
           MOVE "SI-26-1001" TO AN-ORIG-INVOICE
           MOVE 1 TO AN-LINE-CNT
           MOVE 0 TO AN-L-ORIG-LINE (1)
           MOVE "Agreed price adjustment" TO AN-L-DESC (1)
           MOVE 1 TO AN-L-QTY (1)
           MOVE 10.00 TO AN-L-NET (1)
           MOVE "S" TO AN-L-VAT-CODE (1).

      *>   AR-001.  An account is only complete when the financial,
      *>   tax, address and credit information is all on file, and
      *>   CUSTVAL has to say which pieces are missing rather than
      *>   simply refusing.
       1000-ACCOUNT-COMPLETENESS.
           PERFORM 8300-PRIME-CUSTVAL
           MOVE "VALIDATE" TO CA-FUNCTION OF WS-CV-CA
           CALL "CUSTVAL" USING WS-CV-CA
           MOVE "VALIDATE reaches a decision on a seeded account"
               TO TK-NAME
           IF CV-DECISION = "OK" OR CV-DECISION = "INCOMPLETE"
              OR CV-DECISION = "BLOCKED"
               PERFORM TK-PASS
           ELSE
               MOVE CV-DECISION TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "A complete account names no missing information"
               TO TK-NAME
           IF CV-DECISION = "OK"
               MOVE 0 TO TK-EXPECTED-N
           ELSE
               MOVE 1 TO TK-EXPECTED-N
           END-IF
           IF CV-MISSING-CNT > 0
               MOVE 1 TO TK-ACTUAL-N
           ELSE
               MOVE 0 TO TK-ACTUAL-N
           END-IF
           PERFORM TK-ASSERT-NUM
           MOVE "An incomplete account is refused with CUS0010"
               TO TK-NAME
           IF CV-DECISION = "INCOMPLETE"
               MOVE "CUS0010" TO TK-EXPECTED-X
               MOVE CA-REASON-CODE OF WS-CV-CA TO TK-ACTUAL-X
           ELSE
               MOVE "n/a" TO TK-EXPECTED-X
               MOVE "n/a" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR
           PERFORM 8300-PRIME-CUSTVAL
           MOVE "WIBBLE" TO CA-FUNCTION OF WS-CV-CA
           CALL "CUSTVAL" USING WS-CV-CA
           MOVE "An unknown lifecycle step is refused with CUS0098"
               TO TK-NAME
           MOVE "CUS0098" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CV-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AR-001.  Suspension and retirement are audited decisions,
      *>   so neither may be taken without a recorded reason.
       1100-STATUS-NEEDS-REASON.
           PERFORM 8300-PRIME-CUSTVAL
           MOVE "SUSPEND" TO CA-FUNCTION OF WS-CV-CA
           MOVE SPACES TO CV-STATUS-REASON
           CALL "CUSTVAL" USING WS-CV-CA
           MOVE "Suspension without a reason is refused with CUS0014"
               TO TK-NAME
           MOVE "CUS0014" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CV-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "A refused suspension carries an error return code"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CV-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 8300-PRIME-CUSTVAL
           MOVE "RETIRE" TO CA-FUNCTION OF WS-CV-CA
           MOVE SPACES TO CV-STATUS-REASON
           CALL "CUSTVAL" USING WS-CV-CA
           MOVE "Retirement without a reason is refused too"
               TO TK-NAME
           MOVE "CUS0014" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CV-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AR-001.  Retiring an account closes it to new business but
      *>   the outstanding items stay on the ledger and stay
      *>   collectable - a retirement is not a write off.
       1200-RETIRE-KEEPS-THE-DEBT.
           PERFORM 8000-BEGIN-TRAN
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(20)) FROM "
                  "ar.OpenItem WHERE CompanyCode = 'STC' AND "
                  "CustomerCode = 'C0005' AND ResidualAmountGBP <> 0"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8200-ONE-VALUE
           COMPUTE WS-ITEMS-BEFORE = FUNCTION NUMVAL(WS-R1)
           PERFORM 8300-PRIME-CUSTVAL
           MOVE "C0005" TO CV-CUSTOMER
           MOVE "RETIRE" TO CA-FUNCTION OF WS-CV-CA
           MOVE "Account closed at the customer's request"
               TO CV-STATUS-REASON
           CALL "CUSTVAL" USING WS-CV-CA
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(20)) FROM "
                  "ar.OpenItem WHERE CompanyCode = 'STC' AND "
                  "CustomerCode = 'C0005' AND ResidualAmountGBP <> 0"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8200-ONE-VALUE
           COMPUTE WS-ITEMS-AFTER = FUNCTION NUMVAL(WS-R1)
           MOVE "Retiring an account leaves every open item in place"
               TO TK-NAME
           MOVE WS-ITEMS-BEFORE TO TK-EXPECTED-N
           MOVE WS-ITEMS-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 8100-ROLLBACK-TRAN.

      *>   AR-002.  The invoiced, paying and statement parties are
      *>   separate approved relationships.  An account cannot be its
      *>   own related party and only the three known roles exist.
       2000-PARTY-ROLES.
           PERFORM 8300-PRIME-CUSTVAL
           MOVE "RELATE" TO CA-FUNCTION OF WS-CV-CA
           MOVE "PAYER" TO CV-RELATION-ROLE
           MOVE SPACES TO CV-RELATION-CUST
           CALL "CUSTVAL" USING WS-CV-CA
           MOVE "A relationship without a counterparty is refused"
               TO TK-NAME
           MOVE "CUS0020" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CV-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8300-PRIME-CUSTVAL
           MOVE "RELATE" TO CA-FUNCTION OF WS-CV-CA
           MOVE "PAYER" TO CV-RELATION-ROLE
           MOVE "C0001" TO CV-RELATION-CUST
           CALL "CUSTVAL" USING WS-CV-CA
           MOVE "An account cannot be its own paying party"
               TO TK-NAME
           MOVE "CUS0021" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CV-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8300-PRIME-CUSTVAL
           MOVE "RELATE" TO CA-FUNCTION OF WS-CV-CA
           MOVE "GUARANTOR" TO CV-RELATION-ROLE
           MOVE "C0002" TO CV-RELATION-CUST
           CALL "CUSTVAL" USING WS-CV-CA
           MOVE "Only invoiced, paying and statement roles exist"
               TO TK-NAME
           MOVE "CUS0024" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CV-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AR-002.  Naming a parent as the paying party is a routing
      *>   instruction, not a transfer of debt: the other account's
      *>   ledger must be exactly where it was.
       2100-PAYER-DOES-NOT-POOL.
           PERFORM 8000-BEGIN-TRAN
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(SUM(ResidualAmountGBP), 0) AS "
                  "varchar(30)) FROM ar.OpenItem WHERE CompanyCode "
                  "= 'STC' AND CustomerCode = 'C0002'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8200-ONE-VALUE
           COMPUTE WS-DEBT-BEFORE = FUNCTION NUMVAL(WS-R1)
           PERFORM 8300-PRIME-CUSTVAL
           MOVE "RELATE" TO CA-FUNCTION OF WS-CV-CA
           MOVE "PAYER" TO CV-RELATION-ROLE
           MOVE "C0002" TO CV-RELATION-CUST
           CALL "CUSTVAL" USING WS-CV-CA
           MOVE "A paying party relationship is accepted" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE CV-RELATION-OK TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(SUM(ResidualAmountGBP), 0) AS "
                  "varchar(30)) FROM ar.OpenItem WHERE CompanyCode "
                  "= 'STC' AND CustomerCode = 'C0002'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8200-ONE-VALUE
           COMPUTE WS-DEBT-AFTER = FUNCTION NUMVAL(WS-R1)
           MOVE "Naming a payer moves no debt off the other account"
               TO TK-NAME
           MOVE WS-DEBT-BEFORE TO TK-EXPECTED-N
           MOVE WS-DEBT-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
      *>   AR-002.  The roles held on the account are reported back,
      *>   on the instruction that sets them and on any later read.
           MOVE "The paying party is reported on the relate"
               TO TK-NAME
           MOVE "C0002" TO TK-EXPECTED-X
           MOVE CV-PAYER-CUST TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8300-PRIME-CUSTVAL
           MOVE "RELATE" TO CA-FUNCTION OF WS-CV-CA
           MOVE "STATEMENT" TO CV-RELATION-ROLE
           MOVE "C0002" TO CV-RELATION-CUST
           CALL "CUSTVAL" USING WS-CV-CA
           MOVE "The statement party is reported on the relate"
               TO TK-NAME
           MOVE "C0002" TO TK-EXPECTED-X
           MOVE CV-STATEMENT-CUST TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8300-PRIME-CUSTVAL
           MOVE "VALIDATE" TO CA-FUNCTION OF WS-CV-CA
           CALL "CUSTVAL" USING WS-CV-CA
           MOVE "A later read returns the paying party held"
               TO TK-NAME
           MOVE "C0002" TO TK-EXPECTED-X
           MOVE CV-PAYER-CUST TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "A later read returns the statement party held"
               TO TK-NAME
           MOVE "C0002" TO TK-EXPECTED-X
           MOVE CV-STATEMENT-CUST TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(InvoiceCustomerCode, '') FROM "
                  "ar.Customer WHERE CompanyCode = 'STC' AND "
                  "CustomerCode = 'C0001'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8200-ONE-VALUE
           MOVE "The invoiced party read back is the one held"
               TO TK-NAME
           MOVE WS-R1 TO TK-EXPECTED-X
           MOVE CV-INVOICE-CUST TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8100-ROLLBACK-TRAN.

      *>   AR-006.  A financial credit is a money document.  Asking
      *>   it to put the goods back is refused outright - stock comes
      *>   back on a return, with its own movement.
       3000-CREDIT-RETURNS-NO-STOCK.
           PERFORM 8400-PRIME-CREDIT
           MOVE "CHECK" TO CA-FUNCTION OF WS-CN-CA
           MOVE "Y" TO AN-RETURN-STOCK
           CALL "ARCRNOTE" USING WS-CN-CA
           MOVE "A credit note may not return stock (ARC0003)"
               TO TK-NAME
           MOVE "ARC0003" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CN-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "The stock returning credit note is an" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CN-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "No credit note was created for a refused request"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE AN-CREDIT-ID TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AR-006.  Except for an explicit goodwill gesture a credit
      *>   has to say which invoice it is crediting, and it always
      *>   has to say why.
       3100-CREDIT-NAMES-ITS-INVOICE.
           PERFORM 8400-PRIME-CREDIT
           MOVE "CHECK" TO CA-FUNCTION OF WS-CN-CA
           MOVE SPACES TO AN-ORIG-INVOICE
           CALL "ARCRNOTE" USING WS-CN-CA
           MOVE "A value credit must name the original invoice"
               TO TK-NAME
           MOVE "ARC0005" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CN-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8400-PRIME-CREDIT
           MOVE "CHECK" TO CA-FUNCTION OF WS-CN-CA
           MOVE SPACES TO AN-REASON
           CALL "ARCRNOTE" USING WS-CN-CA
           MOVE "A credit note without a reason is refused"
               TO TK-NAME
           MOVE "ARC0002" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CN-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8400-PRIME-CREDIT
           MOVE "CHECK" TO CA-FUNCTION OF WS-CN-CA
           MOVE 0 TO AN-LINE-CNT
           CALL "ARCRNOTE" USING WS-CN-CA
           MOVE "A credit note with no lines is refused" TO TK-NAME
           MOVE "ARC0006" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CN-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AR-006.  The remaining creditable amount is the invoice
      *>   gross less everything already credited against it, a
      *>   credit may not exceed it, and asking the question must not
      *>   change the invoice.
       3200-REMAINING-CREDITABLE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(GrossAmountGBP AS varchar(30)) FROM "
                  "ar.SalesInvoice WHERE CompanyCode = 'STC' AND "
                  "InvoiceNumber = 'SI-26-1001'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8200-ONE-VALUE
           COMPUTE WS-GROSS-BEFORE = FUNCTION NUMVAL(WS-R1)
           PERFORM 8400-PRIME-CREDIT
           MOVE "CHECK" TO CA-FUNCTION OF WS-CN-CA
           CALL "ARCRNOTE" USING WS-CN-CA
           MOVE "The original invoice gross is read back" TO TK-NAME
           MOVE WS-GROSS-BEFORE TO TK-EXPECTED-N
           MOVE AN-ORIG-GROSS-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           COMPUTE WS-EXPECT = AN-ORIG-GROSS-GBP - AN-PRIOR-CREDIT
           MOVE "Remaining creditable is gross less prior credits"
               TO TK-NAME
           MOVE WS-EXPECT TO TK-EXPECTED-N
           MOVE AN-CREDITABLE-REM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(GrossAmountGBP AS varchar(30)) FROM "
                  "ar.SalesInvoice WHERE CompanyCode = 'STC' AND "
                  "InvoiceNumber = 'SI-26-1001'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8200-ONE-VALUE
           COMPUTE WS-GROSS-AFTER = FUNCTION NUMVAL(WS-R1)
           MOVE "Crediting does not edit the original invoice"
               TO TK-NAME
           MOVE WS-GROSS-BEFORE TO TK-EXPECTED-N
           MOVE WS-GROSS-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 8400-PRIME-CREDIT
           MOVE "CHECK" TO CA-FUNCTION OF WS-CN-CA
           MOVE 99999999.00 TO AN-L-NET (1)
           CALL "ARCRNOTE" USING WS-CN-CA
           MOVE "A credit beyond the remaining amount is refused"
               TO TK-NAME
           MOVE "ARC0030" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CN-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       COPY TESTKITP.
