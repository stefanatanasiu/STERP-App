      *>================================================================
      *> PROGRAM-ID : CUSTVAL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : SALES LEDGER ACCOUNT LIFECYCLE.  AN ACCOUNT MAY
      *>              ONLY BE ACTIVATED WHEN THE FINANCIAL, TAX,
      *>              ADDRESS AND CREDIT INFORMATION IS COMPLETE AND
      *>              THE EXTERNAL VERIFICATION EVIDENCE IS ON FILE.
      *>              ORDERING, INVOICED, PAYING AND STATEMENT PARTIES
      *>              ARE SEPARATE ROLES; RECEIVING FUNDS FROM A
      *>              PARENT DOES NOT MOVE ANOTHER DEBTOR'S DEBT.
      *> CALLED BY  : CUSTOMER MAINTENANCE SCREEN, SALES ORDER ENTRY,
      *>              AR DEMO BATCH
      *> INTERFACE  : CALL "CUSTVAL" USING WS-CUSTVAL-CA
      *>              COPYBOOK CUSTVALCA
      *> CALLS      : SQLIO, PERMCHK, AUDITLOG, EXCPRAIS, PARMGET
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-04-16 RJH  ORIGINAL MVS SALES LEDGER ACCOUNT CONTROL
      *> 1998-06-01 DJM  CR0921 PAYING PARTY SEPARATED FROM DEBTOR
      *> 2018-05-25 KMP  CR2301 PERSONAL DATA FLAG FOR UK GDPR
      *> 2026-10-05 STERP COMPANIES HOUSE, HMRC AND SANCTIONS
      *> 2026-10-07 STERP CR2615 THE INVOICE, PAYER AND STATEMENT
      *>                 PARTIES ARE READ BACK ON EVERY FUNCTION
      *>================================================================
      *> REQ: AR-001, AR-002, AR-028
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTVAL.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 16/04/1991.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ARWORK.
       COPY PERMCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-PERM-CA==.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUDIT-CA==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXCP-CA==.
       01  WS-CUST.
           05  WS-CU-NAME              PIC X(120).
           05  WS-CU-FORM              PIC X(14).
           05  WS-CU-CHNO              PIC X(10).
           05  WS-CU-CHSTAT            PIC X(20).
           05  WS-CU-VATLOC            PIC X(3).
           05  WS-CU-VATNO             PIC X(15).
           05  WS-CU-VATVAL            PIC X(10).
           05  WS-CU-TERMS             PIC X(12).
           05  WS-CU-CONTROL           PIC X(12).
           05  WS-CU-CURRENCY          PIC X(3).
           05  WS-CU-STATUS            PIC X(10).
           05  WS-CU-SANCTIONS         PIC X(12).
           05  WS-CU-ADDR-REG          PIC X(4).
           05  WS-CU-ADDR-BILL         PIC X(4).
           05  WS-CU-LIMIT             PIC X(20).
           05  WS-CU-PERSONAL          PIC X(2).
           05  WS-CU-INVCUST           PIC X(20).
           05  WS-CU-PAYCUST           PIC X(20).
           05  WS-CU-STATCUST          PIC X(20).
       01  WS-REL-ROW.
           05  WS-RL-EXISTS            PIC X(4).
           05  WS-RL-STATUS            PIC X(10).
           05  WS-RL-NAME              PIC X(120).
           05  WS-RL-SPARE             PIC X(10).
       01  WS-SCREEN-ROW.
           05  WS-SC-ID                PIC X(20).
           05  WS-SC-CO                PIC X(10).
           05  WS-SC-PTYPE             PIC X(12).
           05  WS-SC-PCODE             PIC X(20).
           05  WS-SC-PNAME             PIC X(120).
           05  WS-SC-STATUS            PIC X(12).
           05  WS-SC-ENTRY             PIC X(12).
           05  WS-SC-MATCHED           PIC X(120).
           05  WS-SC-SCORE             PIC X(10).
       01  WS-SW.
           05  WS-OLD-STATUS           PIC X(10) VALUE SPACES.
       LINKAGE SECTION.
       COPY CUSTVALCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE-INPUT
           IF AR-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0300-READ-ACCOUNT
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE AR-FUNC
               WHEN "VALIDATE" PERFORM 1000-CHECK-COMPLETE
               WHEN "ACTIVATE" PERFORM 1000-CHECK-COMPLETE
                               PERFORM 2000-ACTIVATE
               WHEN "SUSPEND"  PERFORM 2200-CHANGE-STATUS
               WHEN "RETIRE"   PERFORM 2200-CHANGE-STATUS
               WHEN "RELATE"   PERFORM 3000-RELATIONSHIP
               WHEN "SCREEN"   PERFORM 4000-EXTERNAL-CHECKS
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           PERFORM 9900-SET-RESULT
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE AR-WORK
           INITIALIZE AR-CTL
           INITIALIZE WS-CUST
           INITIALIZE WS-REL-ROW
           INITIALIZE WS-SCREEN-ROW
           MOVE CA-FUNCTION OF DFHCOMMAREA TO AR-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO AR-CO
           MOVE CA-SITE OF DFHCOMMAREA TO AR-SITE
           MOVE CA-USER OF DFHCOMMAREA TO AR-USERID
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO AR-BUSDATE
           MOVE "CUSTVAL" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO AR-RC AR-MSG-CNT
           MOVE SPACES TO AR-REASON AR-MSG
           MOVE "AR-001" TO AR-REQ-REF
           MOVE 0 TO CV-MISSING-CNT CV-EXCEPTION-ID CV-SCREEN-ID
           MOVE 0 TO CV-SANCTIONS-HITS
           MOVE SPACES TO CV-DECISION
           MOVE "N" TO CV-RELATION-OK
           MOVE FUNCTION UPPER-CASE(CV-CUSTOMER) TO CV-CUSTOMER
           IF AR-BUSDATE = 0
               MOVE FUNCTION CURRENT-DATE TO AR-NOW
               MOVE AR-NOW(1:8) TO AR-BUSDATE
           END-IF.
       0100-EXIT.
           EXIT.

       0200-VALIDATE-INPUT SECTION.
       0200-START.
           IF AR-CO = SPACES
               MOVE "CUS0001" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A company code is required" DELIMITED BY SIZE
                   INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CV-CUSTOMER = SPACES
               MOVE "CUS0002" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A customer account reference is required"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

       0300-READ-ACCOUNT SECTION.
       0300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT c.LegalName, c.LegalForm, "
                  "ISNULL(c.CompaniesHouseNo,''), "
                  "ISNULL(c.CompaniesHouseStatus,''), c.VatLocation, "
                  "ISNULL(c.VatRegNo,''), "
                  "ISNULL(CONVERT(char(8), c.VatValidatedAt, 112),"
                  "''), ISNULL(c.TermsCode,''), "
                  "ISNULL(c.ArControlAccount,''), c.CurrencyCode, "
                  "c.Status, c.SanctionsStatus, "
                  "CONVERT(varchar(4),(SELECT COUNT(*) FROM "
                  "ar.CustomerAddress a WHERE a.CompanyCode = "
                  "c.CompanyCode AND a.CustomerCode = c.CustomerCode "
                  "AND a.AddressRole = 'REGISTERED' AND "
                  "LTRIM(RTRIM(ISNULL(a.Line1,''))) <> '')), "
                  "CONVERT(varchar(4),(SELECT COUNT(*) FROM "
                  "ar.CustomerAddress a WHERE a.CompanyCode = "
                  "c.CompanyCode AND a.CustomerCode = c.CustomerCode "
                  "AND a.AddressRole = 'BILLING' AND "
                  "LTRIM(RTRIM(ISNULL(a.Line1,''))) <> '')), "
                  "CONVERT(varchar(20), ISNULL((SELECT "
                  "k.CreditLimitGBP FROM ar.CustomerCredit k WHERE "
                  "k.CompanyCode = c.CompanyCode AND k.CustomerCode "
                  "= c.CustomerCode),0)), "
                  "CONVERT(char(1), c.IsPersonalData), "
                  "ISNULL(c.InvoiceCustomerCode,''), "
                  "ISNULL(c.PayerCustomerCode,''), "
                  "ISNULL(c.StatementCustomerCode,'') "
                  "FROM ar.Customer c WHERE c.CompanyCode = ? "
                  "AND c.CustomerCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE CV-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 19 TO SQLIO-COL-CNT
           MOVE 120 TO SQLIO-COL-WIDTH(1)
           MOVE 14 TO SQLIO-COL-WIDTH(2)
           MOVE 10 TO SQLIO-COL-WIDTH(3)
           MOVE 20 TO SQLIO-COL-WIDTH(4)
           MOVE 3 TO SQLIO-COL-WIDTH(5)
           MOVE 15 TO SQLIO-COL-WIDTH(6)
           MOVE 10 TO SQLIO-COL-WIDTH(7)
           MOVE 12 TO SQLIO-COL-WIDTH(8)
           MOVE 12 TO SQLIO-COL-WIDTH(9)
           MOVE 3 TO SQLIO-COL-WIDTH(10)
           MOVE 10 TO SQLIO-COL-WIDTH(11)
           MOVE 12 TO SQLIO-COL-WIDTH(12)
           MOVE 4 TO SQLIO-COL-WIDTH(13)
           MOVE 4 TO SQLIO-COL-WIDTH(14)
           MOVE 20 TO SQLIO-COL-WIDTH(15)
           MOVE 2 TO SQLIO-COL-WIDTH(16)
           MOVE 20 TO SQLIO-COL-WIDTH(17)
           MOVE 20 TO SQLIO-COL-WIDTH(18)
           MOVE 20 TO SQLIO-COL-WIDTH(19)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CUST
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "CUS0003" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Customer " FUNCTION TRIM(CV-CUSTOMER)
                      " is not held for company "
                      FUNCTION TRIM(AR-CO)
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 0300-EXIT
           END-IF
           MOVE WS-CU-NAME TO CV-LEGAL-NAME
           MOVE WS-CU-FORM TO CV-LEGAL-FORM
           MOVE WS-CU-CHNO TO CV-CH-NUMBER
           MOVE WS-CU-CHSTAT TO CV-CH-STATUS
           MOVE WS-CU-VATLOC TO CV-VAT-LOCATION
           MOVE WS-CU-VATNO TO CV-VAT-REG-NO
           MOVE WS-CU-TERMS TO CV-TERMS-CODE
           MOVE WS-CU-CONTROL TO CV-CONTROL-ACCOUNT
           MOVE WS-CU-CURRENCY TO CV-CURRENCY
           MOVE WS-CU-STATUS TO CV-STATUS
           MOVE WS-CU-STATUS TO WS-OLD-STATUS
           MOVE WS-CU-SANCTIONS TO CV-SANCTIONS-STATUS
           MOVE WS-CU-PERSONAL(1:1) TO CV-PERSONAL-DATA
      *>   AR-002 - the party relationships held on the account are
      *>   reported on every read, so a caller knows who is invoiced,
      *>   who pays and who is statemented before it trades
           MOVE WS-CU-INVCUST TO CV-INVOICE-CUST
           MOVE WS-CU-PAYCUST TO CV-PAYER-CUST
           MOVE WS-CU-STATCUST TO CV-STATEMENT-CUST
           COMPUTE CV-CREDIT-LIMIT = FUNCTION NUMVAL(WS-CU-LIMIT)
           IF WS-CU-VATVAL NOT = SPACES
               MOVE "Y" TO CV-VAT-VALIDATED
           ELSE
               MOVE "N" TO CV-VAT-VALIDATED
           END-IF
           IF WS-CU-ADDR-REG NOT = SPACES
              AND FUNCTION NUMVAL(WS-CU-ADDR-REG) > 0
               MOVE "Y" TO CV-HAS-REGISTERED
           ELSE
               MOVE "N" TO CV-HAS-REGISTERED
           END-IF
           IF WS-CU-ADDR-BILL NOT = SPACES
              AND FUNCTION NUMVAL(WS-CU-ADDR-BILL) > 0
               MOVE "Y" TO CV-HAS-BILLING
           ELSE
               MOVE "N" TO CV-HAS-BILLING
           END-IF
           IF CV-INDIVIDUAL
               MOVE "Y" TO CV-PERSONAL-DATA
           END-IF.
       0300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   AR-001 - the required financial, tax, address and credit
      *>   information.  An incomplete account cannot be activated.
       1000-CHECK-COMPLETE SECTION.
       1000-START.
           MOVE 0 TO CV-MISSING-CNT
           IF CV-LEGAL-NAME = SPACES
               PERFORM 1010-MISS-NAME
           END-IF
           IF CV-HAS-REGISTERED = "N"
               PERFORM 1020-MISS-REG-ADDRESS
           END-IF
           IF CV-HAS-BILLING = "N"
               PERFORM 1030-MISS-BILL-ADDRESS
           END-IF
           IF CV-TERMS-CODE = SPACES
               PERFORM 1040-MISS-TERMS
           END-IF
           IF CV-CONTROL-ACCOUNT = SPACES
               PERFORM 1050-MISS-CONTROL
           END-IF
           IF CV-CURRENCY = SPACES
               PERFORM 1060-MISS-CURRENCY
           END-IF
           IF CV-LEGAL-FORM = "LTD" OR CV-LEGAL-FORM = "PLC"
               IF CV-CH-NUMBER = SPACES
                   PERFORM 1070-MISS-CHNO
               END-IF
           END-IF
      *>   AR-028: a customer claiming VAT registration needs a
      *>   validated number or an authorised exception
           IF CV-VAT-REG-NO NOT = SPACES
              AND CV-VAT-VALIDATED NOT = "Y"
              AND CV-VAT-EXCEPT-REF = SPACES
               PERFORM 1080-MISS-VAT-VALIDATION
           END-IF
           IF CV-MISSING-CNT > 0
               SET CV-DEC-INCOMPLETE TO TRUE
               MOVE 8 TO AR-RC
               MOVE "CUS0010" TO AR-REASON
               MOVE CV-MISSING-CNT TO AR-INT-ED
               MOVE SPACES TO AR-MSG
               STRING "The account is incomplete: "
                      FUNCTION TRIM(AR-INT-ED)
                      " required item(s) are missing, starting with "
                      FUNCTION TRIM(CV-MISSING(1))
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           ELSE
               SET CV-DEC-OK TO TRUE
               MOVE SPACES TO AR-MSG
               STRING "Account " FUNCTION TRIM(CV-CUSTOMER)
                      " carries the required information"
                   DELIMITED BY SIZE INTO AR-MSG
           END-IF.
       1000-EXIT.
           EXIT.

       1010-MISS-NAME SECTION.
       1010-START.
           ADD 1 TO CV-MISSING-CNT
           MOVE "Legal name" TO CV-MISSING(CV-MISSING-CNT).
       1010-EXIT.
           EXIT.

       1020-MISS-REG-ADDRESS SECTION.
       1020-START.
           ADD 1 TO CV-MISSING-CNT
           MOVE "Registered office address"
               TO CV-MISSING(CV-MISSING-CNT).
       1020-EXIT.
           EXIT.

       1030-MISS-BILL-ADDRESS SECTION.
       1030-START.
           ADD 1 TO CV-MISSING-CNT
           MOVE "Billing address" TO CV-MISSING(CV-MISSING-CNT).
       1030-EXIT.
           EXIT.

       1040-MISS-TERMS SECTION.
       1040-START.
           ADD 1 TO CV-MISSING-CNT
           MOVE "Payment terms" TO CV-MISSING(CV-MISSING-CNT).
       1040-EXIT.
           EXIT.

       1050-MISS-CONTROL SECTION.
       1050-START.
           ADD 1 TO CV-MISSING-CNT
           MOVE "Receivables control account"
               TO CV-MISSING(CV-MISSING-CNT).
       1050-EXIT.
           EXIT.

       1060-MISS-CURRENCY SECTION.
       1060-START.
           ADD 1 TO CV-MISSING-CNT
           MOVE "Trading currency" TO CV-MISSING(CV-MISSING-CNT).
       1060-EXIT.
           EXIT.

       1070-MISS-CHNO SECTION.
       1070-START.
           ADD 1 TO CV-MISSING-CNT
           MOVE "Companies House number"
               TO CV-MISSING(CV-MISSING-CNT).
       1070-EXIT.
           EXIT.

       1080-MISS-VAT-VALIDATION SECTION.
       1080-START.
           ADD 1 TO CV-MISSING-CNT
           MOVE "Validated VAT registration number"
               TO CV-MISSING(CV-MISSING-CNT).
       1080-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       2000-ACTIVATE SECTION.
       2000-START.
           IF AR-RC-BAD
               GO TO 2000-EXIT
           END-IF
           PERFORM 2050-CHECK-AUTHORITY
           IF AR-RC-BAD
               GO TO 2000-EXIT
           END-IF
           IF CV-CH-STATUS NOT = SPACES
              AND CV-CH-STATUS NOT = "ACTIVE"
               MOVE 8 TO AR-RC
               MOVE "CUS0011" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Companies House shows this company as "
                      FUNCTION TRIM(CV-CH-STATUS)
                      "; activation is blocked"
                   DELIMITED BY SIZE INTO AR-MSG
               SET CV-DEC-BLOCKED TO TRUE
               PERFORM 8000-RAISE-EXCEPTION
               GO TO 2000-EXIT
           END-IF
           IF CV-SANCTIONS-STATUS = "POTENTIAL"
              OR CV-SANCTIONS-STATUS = "MATCH"
               MOVE 6 TO AR-RC
               MOVE "CUS0012" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A sanctions screening match must be decided "
                      "by an authorised reviewer before activation"
                   DELIMITED BY SIZE INTO AR-MSG
               SET CV-DEC-BLOCKED TO TRUE
               PERFORM 8000-RAISE-EXCEPTION
               GO TO 2000-EXIT
           END-IF
           MOVE "ACTIVE" TO CV-NEW-STATUS
           PERFORM 2300-WRITE-STATUS
           IF AR-RC-BAD
               GO TO 2000-EXIT
           END-IF
           MOVE SPACES TO AR-MSG
           STRING "Account " FUNCTION TRIM(CV-CUSTOMER)
                  " activated"
               DELIMITED BY SIZE INTO AR-MSG.
       2000-EXIT.
           EXIT.

       2050-CHECK-AUTHORITY SECTION.
       2050-START.
           INITIALIZE WS-PERM-CA
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PERM-CA
           MOVE AR-SITE TO CA-SITE OF WS-PERM-CA
           MOVE AR-USERID TO CA-USER OF WS-PERM-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-PERM-CA
           IF AR-FUNC = "ACTIVATE"
               MOVE "AR.CUSTOMER.APPROVE" TO PK-PERMISSION
           ELSE
               MOVE "AR.CUSTOMER.MAINT" TO PK-PERMISSION
           END-IF
           CALL "PERMCHK" USING WS-PERM-CA
           IF CA-RETURN-CODE OF WS-PERM-CA > 4
               MOVE 6 TO AR-RC
               MOVE "CUS0013" TO AR-REASON
               MOVE CA-MESSAGE OF WS-PERM-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       2050-EXIT.
           EXIT.

      *>   SUSPEND / RETIRE.  A retired account with outstanding items
      *>   remains payable, refundable and collectable (AR-021).
       2200-CHANGE-STATUS SECTION.
       2200-START.
           PERFORM 2050-CHECK-AUTHORITY
           IF AR-RC-BAD
               GO TO 2200-EXIT
           END-IF
           IF CV-STATUS-REASON = SPACES
               MOVE 8 TO AR-RC
               MOVE "CUS0014" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A reason is required to suspend or retire an "
                      "account" DELIMITED BY SIZE INTO AR-MSG
               GO TO 2200-EXIT
           END-IF
           IF AR-FUNC = "SUSPEND"
               MOVE "SUSPENDED" TO CV-NEW-STATUS
           ELSE
               MOVE "RETIRED" TO CV-NEW-STATUS
           END-IF
           PERFORM 2300-WRITE-STATUS
           IF AR-RC-BAD
               GO TO 2200-EXIT
           END-IF
           MOVE SPACES TO AR-MSG
           STRING "Account " FUNCTION TRIM(CV-CUSTOMER) " is now "
                  FUNCTION TRIM(CV-NEW-STATUS)
                  "; outstanding items remain collectable"
               DELIMITED BY SIZE INTO AR-MSG.
       2200-EXIT.
           EXIT.

       2300-WRITE-STATUS SECTION.
       2300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.Customer SET Status = ?, "
                  "StatusReason = ?, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE CompanyCode = ? "
                  "AND CustomerCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CV-NEW-STATUS TO SQLIO-PARM-VAL(1)
           MOVE CV-STATUS-REASON TO SQLIO-PARM-VAL(2)
           MOVE AR-USERID TO SQLIO-PARM-VAL(3)
           MOVE AR-CO TO SQLIO-PARM-VAL(4)
           MOVE CV-CUSTOMER TO SQLIO-PARM-VAL(5)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CUST
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2300-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CUST
           MOVE CV-NEW-STATUS TO CV-STATUS
           PERFORM 8100-AUDIT-STATUS.
       2300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   AR-002 - an approved relationship between two accounts of
      *>   the same company.  The roles stay explicit, and funds from
      *>   a parent never transfer another debtor's obligation.
       3000-RELATIONSHIP SECTION.
       3000-START.
           PERFORM 2050-CHECK-AUTHORITY
           IF AR-RC-BAD
               GO TO 3000-EXIT
           END-IF
           IF CV-RELATION-CUST = SPACES
               MOVE 8 TO AR-RC
               MOVE "CUS0020" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A related account is required" DELIMITED BY
                   SIZE INTO AR-MSG
               GO TO 3000-EXIT
           END-IF
           IF CV-RELATION-CUST = CV-CUSTOMER
               MOVE 8 TO AR-RC
               MOVE "CUS0021" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "An account cannot be its own invoiced, paying "
                      "or statement party"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 3000-EXIT
           END-IF
           PERFORM 3100-READ-RELATED
           IF AR-RC-BAD
               GO TO 3000-EXIT
           END-IF
           PERFORM 3200-WRITE-RELATION.
       3000-EXIT.
           EXIT.

       3100-READ-RELATED SECTION.
       3100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT '1', c.Status, c.LegalName, '' "
                  "FROM ar.Customer c WHERE c.CompanyCode = ? "
                  "AND c.CustomerCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE CV-RELATION-CUST TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-COL-WIDTH(1)
           MOVE 10 TO SQLIO-COL-WIDTH(2)
           MOVE 120 TO SQLIO-COL-WIDTH(3)
           MOVE 10 TO SQLIO-COL-WIDTH(4)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-REL-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 3100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "CUS0022" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Related account "
                      FUNCTION TRIM(CV-RELATION-CUST)
                      " is not held for this company - a relationship"
                      " never crosses a legal company"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 3100-EXIT
           END-IF
           IF WS-RL-STATUS = "RETIRED"
               MOVE 8 TO AR-RC
               MOVE "CUS0023" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A retired account cannot take a new party role"
                   DELIMITED BY SIZE INTO AR-MSG
           END-IF.
       3100-EXIT.
           EXIT.

       3200-WRITE-RELATION SECTION.
       3200-START.
           MOVE SPACES TO SQLIO-STMT
           EVALUATE TRUE
               WHEN CV-ROLE-INVOICE
                   STRING "UPDATE ar.Customer SET InvoiceCustomerCode"
                          " = ?, ModifiedBy = ?, ModifiedAt = "
                          "SYSDATETIME() WHERE CompanyCode = ? AND "
                          "CustomerCode = ?"
                       DELIMITED BY SIZE INTO SQLIO-STMT
               WHEN CV-ROLE-PAYER
                   STRING "UPDATE ar.Customer SET PayerCustomerCode "
                          "= ?, ModifiedBy = ?, ModifiedAt = "
                          "SYSDATETIME() WHERE CompanyCode = ? AND "
                          "CustomerCode = ?"
                       DELIMITED BY SIZE INTO SQLIO-STMT
               WHEN CV-ROLE-STATE
                   STRING "UPDATE ar.Customer SET "
                          "StatementCustomerCode = ?, ModifiedBy = ?,"
                          " ModifiedAt = SYSDATETIME() WHERE "
                          "CompanyCode = ? AND CustomerCode = ?"
                       DELIMITED BY SIZE INTO SQLIO-STMT
               WHEN OTHER
                   MOVE 8 TO AR-RC
                   MOVE "CUS0024" TO AR-REASON
                   MOVE SPACES TO AR-MSG
                   STRING "The party role must be INVOICE, PAYER or "
                          "STATEMENT" DELIMITED BY SIZE INTO AR-MSG
                   GO TO 3200-EXIT
           END-EVALUATE
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CV-RELATION-CUST TO SQLIO-PARM-VAL(1)
           MOVE AR-USERID TO SQLIO-PARM-VAL(2)
           MOVE AR-CO TO SQLIO-PARM-VAL(3)
           MOVE CV-CUSTOMER TO SQLIO-PARM-VAL(4)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-REL-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 3200-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-REL-ROW
           MOVE "Y" TO CV-RELATION-OK
           EVALUATE TRUE
               WHEN CV-ROLE-INVOICE
                   MOVE CV-RELATION-CUST TO CV-INVOICE-CUST
               WHEN CV-ROLE-PAYER
                   MOVE CV-RELATION-CUST TO CV-PAYER-CUST
               WHEN CV-ROLE-STATE
                   MOVE CV-RELATION-CUST TO CV-STATEMENT-CUST
           END-EVALUATE
           PERFORM 8100-AUDIT-STATUS
           MOVE SPACES TO AR-MSG
           STRING FUNCTION TRIM(CV-RELATION-ROLE) " party of "
                  FUNCTION TRIM(CV-CUSTOMER) " is now "
                  FUNCTION TRIM(CV-RELATION-CUST)
                  "; the legal debtor is unchanged"
               DELIMITED BY SIZE INTO AR-MSG.
       3200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   AR-028 / COM-029 - external verification and sanctions
       4000-EXTERNAL-CHECKS SECTION.
       4000-START.
           PERFORM 4100-SCREEN-PARTY
           IF AR-RC-BAD
               GO TO 4000-EXIT
           END-IF
           PERFORM 4200-RECORD-EVIDENCE
           IF CV-SANCTIONS-HITS > 0
               MOVE 6 TO AR-RC
               MOVE "CUS0030" TO AR-REASON
               MOVE CV-SANCTIONS-HITS TO AR-INT-ED
               MOVE SPACES TO AR-MSG
               STRING "Sanctions screening returned "
                      FUNCTION TRIM(AR-INT-ED)
                      " potential match(es); release, receipts and "
                      "payment are blocked until decided"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 8000-RAISE-EXCEPTION
           ELSE
               MOVE SPACES TO AR-MSG
               STRING "External checks recorded for "
                      FUNCTION TRIM(CV-CUSTOMER) " - sanctions clear"
                   DELIMITED BY SIZE INTO AR-MSG
           END-IF.
       4000-EXIT.
           EXIT.

       4100-SCREEN-PARTY SECTION.
       4100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC core.usp_ScreenParty ?, ?, ?, ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 7 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE "CUSTOMER" TO SQLIO-PARM-VAL(2)
           MOVE CV-CUSTOMER TO SQLIO-PARM-VAL(3)
           MOVE CV-LEGAL-NAME TO SQLIO-PARM-VAL(4)
           MOVE SPACES TO SQLIO-PARM-VAL(5)
           MOVE AR-USERID TO SQLIO-PARM-VAL(6)
           MOVE "ONBOARD" TO SQLIO-PARM-VAL(7)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 7
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE "Y" TO SQLIO-PARM-NULL(5)
           MOVE 9 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 10 TO SQLIO-COL-WIDTH(2)
           MOVE 12 TO SQLIO-COL-WIDTH(3)
           MOVE 20 TO SQLIO-COL-WIDTH(4)
           MOVE 120 TO SQLIO-COL-WIDTH(5)
           MOVE 12 TO SQLIO-COL-WIDTH(6)
           MOVE 12 TO SQLIO-COL-WIDTH(7)
           MOVE 120 TO SQLIO-COL-WIDTH(8)
           MOVE 10 TO SQLIO-COL-WIDTH(9)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SCREEN-ROW
           IF SQLIO-ERROR
      *>       screening is advisory plumbing; a failure is reported
      *>       but does not pretend the party is clear
               MOVE 4 TO AR-RC
               MOVE "CUS0031" TO AR-REASON
               MOVE SQLIO-ERRMSG TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 4100-EXIT
           END-IF
           IF NOT SQLIO-NOTFOUND
               COMPUTE CV-SCREEN-ID = FUNCTION NUMVAL(WS-SC-ID)
               IF WS-SC-STATUS NOT = SPACES
                   MOVE WS-SC-STATUS TO CV-SANCTIONS-STATUS
               END-IF
               IF CV-SANCTIONS-STATUS NOT = "CLEAR"
                   MOVE 1 TO CV-SANCTIONS-HITS
               ELSE
                   MOVE 0 TO CV-SANCTIONS-HITS
               END-IF
           END-IF.
       4100-EXIT.
           EXIT.

       4200-RECORD-EVIDENCE SECTION.
       4200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.Customer SET SanctionsStatus = ?, "
                  "SanctionsScreenedAt = SYSDATETIME(), "
                  "CompaniesHouseStatus = NULLIF(?,''), "
                  "CompaniesHouseCheckedAt = "
                  "CASE WHEN ? = '' THEN CompaniesHouseCheckedAt "
                  "ELSE CONVERT(date, ?) END, "
                  "VatValidatedAt = CASE WHEN ? = 'Y' THEN "
                  "CONVERT(date, ?) ELSE VatValidatedAt END, "
                  "VatValidationRef = CASE WHEN ? = '' THEN "
                  "VatValidationRef ELSE ? END, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() "
                  "WHERE CompanyCode = ? AND CustomerCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 11 TO SQLIO-PARM-CNT
           MOVE CV-SANCTIONS-STATUS TO SQLIO-PARM-VAL(1)
           MOVE CV-CH-STATUS TO SQLIO-PARM-VAL(2)
           IF CV-CH-STATUS = SPACES
               MOVE SPACES TO SQLIO-PARM-VAL(3)
               MOVE SPACES TO SQLIO-PARM-VAL(4)
           ELSE
               MOVE AR-BUSDATE TO SQLIO-PARM-VAL(3)
               MOVE AR-BUSDATE TO SQLIO-PARM-VAL(4)
           END-IF
           MOVE CV-VAT-VALIDATED TO SQLIO-PARM-VAL(5)
           MOVE AR-BUSDATE TO SQLIO-PARM-VAL(6)
           MOVE CV-VAT-VALID-REF TO SQLIO-PARM-VAL(7)
           MOVE CV-VAT-VALID-REF TO SQLIO-PARM-VAL(8)
           MOVE AR-USERID TO SQLIO-PARM-VAL(9)
           MOVE AR-CO TO SQLIO-PARM-VAL(10)
           MOVE CV-CUSTOMER TO SQLIO-PARM-VAL(11)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 11
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CUST
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 4200-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CUST.
       4200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-RAISE-EXCEPTION SECTION.
       8000-START.
           INITIALIZE WS-EXCP-CA
           MOVE "RAISE" TO CA-FUNCTION OF WS-EXCP-CA
           MOVE AR-CO TO CA-COMPANY OF WS-EXCP-CA
           MOVE AR-USERID TO CA-USER OF WS-EXCP-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-EXCP-CA
           MOVE "AR" TO EX-MODULE
           MOVE "HOLD" TO EX-SEVERITY
           MOVE "ONBOARDING" TO EX-CATEGORY
           MOVE AR-MSG TO EX-MESSAGE-TEXT
           MOVE "CUSTOMER" TO EX-DOCUMENT-TYPE
           MOVE CV-CUSTOMER TO EX-DOCUMENT-REF
           MOVE "CREDMGR" TO EX-OWNER-ROLE
           MOVE 1 TO EX-PRIORITY
           CALL "EXCPRAIS" USING WS-EXCP-CA
           MOVE EX-EXCEPTION-ID TO CV-EXCEPTION-ID.
       8000-EXIT.
           EXIT.

       8100-AUDIT-STATUS SECTION.
       8100-START.
           INITIALIZE WS-AUDIT-CA
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUDIT-CA
           MOVE AR-CO TO CA-COMPANY OF WS-AUDIT-CA
           MOVE AR-USERID TO CA-USER OF WS-AUDIT-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-AUDIT-CA
           MOVE "ar.Customer" TO AU-ENTITY-TYPE
           MOVE CV-CUSTOMER TO AU-ENTITY-KEY
           MOVE AR-FUNC TO AU-ACTION
           IF AR-FUNC = "RELATE"
               MOVE CV-RELATION-ROLE TO AU-FIELD-NAME
               MOVE SPACES TO AU-OLD-VALUE
               MOVE CV-RELATION-CUST TO AU-NEW-VALUE
           ELSE
               MOVE "Status" TO AU-FIELD-NAME
               MOVE WS-OLD-STATUS TO AU-OLD-VALUE
               MOVE CV-NEW-STATUS TO AU-NEW-VALUE
           END-IF
           MOVE CV-STATUS-REASON TO AU-REASON
           MOVE "COBOL" TO AU-SOURCE
           CALL "AUDITLOG" USING WS-AUDIT-CA.
       8100-EXIT.
           EXIT.

       9000-REJECT SECTION.
       9000-START.
           MOVE 8 TO AR-RC
           MOVE "Y" TO AR-ERROR
           ADD 1 TO AR-MSG-CNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE 8 TO AR-RC
           MOVE "CUS0098" TO AR-REASON
           MOVE SPACES TO AR-MSG
           STRING "CUSTVAL does not support function ["
                  FUNCTION TRIM(AR-FUNC) "]"
               DELIMITED BY SIZE INTO AR-MSG.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE 12 TO AR-RC
           MOVE "SQL0001" TO AR-REASON
           MOVE SQLIO-ERRMSG TO AR-MSG
           ADD 1 TO AR-MSG-CNT.
       9300-EXIT.
           EXIT.

       9900-SET-RESULT SECTION.
       9900-START.
           MOVE AR-RC TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE AR-REASON TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE AR-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE AR-MSG-CNT TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE AR-REQ-REF TO CA-REQ-REF OF DFHCOMMAREA.
       9900-EXIT.
           EXIT.
