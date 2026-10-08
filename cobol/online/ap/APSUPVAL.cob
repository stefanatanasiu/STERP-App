      *>================================================================
      *> PROGRAM-ID : APSUPVAL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : SUPPLIER IDENTITY, FINANCIAL PROFILE, STATUS AND
      *>              THE TWO SEPARATE HOLDS.  A PURCHASING HOLD STOPS
      *>              NEW COMMITMENT; IT DOES NOT ERASE AN EXISTING
      *>              LIABILITY NOR FORBID AN AUTHORISED PAYMENT OF IT
      *>              (AP-001).  A RETIRED ACCOUNT MAY STILL BE SETTLED
      *>              AND ENQUIRED ON (AP-024).  AGREED TERMS ARE
      *>              CHECKED AGAINST THE STATUTORY AND POLICY MAXIMUM
      *>              FOR THE SUPPLIER'S SIZE (AP-030, D-25).
      *> CALLED BY  : .NET CREDITORS FORMS, APINVREG, APPAYPRP, APBACS
      *> INTERFACE  : CALL "APSUPVAL" USING WS-APSUPVAL-CA
      *>              COPYBOOK APSUPVALCA
      *> CALLS      : SQLIO, PERMCHK, AUDITLOG, PARMGET, TERMCALC
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-03-18 RJH  ORIGINAL MVS PORT FROM THE ICL PURCHASE LEDGER
      *> 1993-06-22 DJM  CR0388 SEPARATE PURCHASE AND PAYMENT HOLDS
      *>                 AFTER THE MIDLANDS SUSPENSION INCIDENT
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS
      *> 2004-02-11 KMP  CR1811 BUSINESS SIZE FOR LATE PAYMENT POLICY
      *> 2021-01-01 AJS  BREXIT - VAT LOCATION GB / XI / EU / ROW
      *> 2026-10-05 STERP SANCTIONS AND DUE DILIGENCE GATE (D-29),
      *>                 STATUTORY TERM LIMIT (D-25)
      *>================================================================
      *> REQ: AP-001, AP-024, AP-030, D-25, D-29
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APSUPVAL.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 18/03/1991.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY APSVCARE.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
           05  WS-FOUND-SW             PIC X VALUE "N".
               88  WS-SUPPLIER-FOUND             VALUE "Y".
           05  WS-DISP                 PIC 9 VALUE 1.
       01  WS-SUPPLIER-ROW.
           05  WS-R-LEGAL-NAME         PIC X(120).
           05  WS-R-TRADING-NAME       PIC X(120).
           05  WS-R-LEGAL-FORM         PIC X(14).
           05  WS-R-STATUS             PIC X(10).
           05  WS-R-SIZE               PIC X(8).
           05  WS-R-CURRENCY           PIC X(3).
           05  WS-R-ALLOWED            PIC X(40).
           05  WS-R-METHOD             PIC X(10).
           05  WS-R-VAT-LOC            PIC X(3).
           05  WS-R-VAT-NO             PIC X(15).
           05  WS-R-COUNTRY            PIC X(2).
           05  WS-R-SANCTIONS          PIC X(12).
           05  WS-R-CIS                PIC X(1).
           05  WS-R-EINV               PIC X(1).
           05  WS-R-CONTROL            PIC X(12).
           05  WS-R-EXPENSE            PIC X(12).
           05  WS-R-COSTCTR            PIC X(20).
           05  WS-R-OWNER              PIC X(20).
           05  WS-R-BUYER              PIC X(20).
           05  WS-R-TERMS              PIC X(12).
           05  WS-R-PHOLD              PIC X(1).
           05  WS-R-YHOLD              PIC X(1).
           05  WS-R-HOLD-REASON        PIC X(200).
           05  WS-R-MINPAY             PIC X(24).
       01  WS-BALANCE-ROW.
           05  WS-B-RESIDUAL           PIC X(30).
           05  WS-B-COUNT              PIC X(12).
       01  WS-DD-ROW.
           05  WS-D-OUTSTANDING        PIC X(12).
           05  WS-D-FAILED             PIC X(12).
       01  WS-WORK.
           05  WS-NUM                  PIC S9(15)V9(6) VALUE 0.
           05  WS-OLD-VALUE            PIC X(100) VALUE SPACES.
           05  WS-NEW-VALUE            PIC X(100) VALUE SPACES.
           05  WS-HOLD-COL             PIC X(14) VALUE SPACES.
           05  WS-SET-FLAG             PIC 9 VALUE 0.
           05  WS-PERM                 PIC X(40) VALUE SPACES.
           05  WS-DAYS-ED              PIC -(5)9.
       LINKAGE SECTION.
       COPY APSUPVALCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           PERFORM 1500-VALIDATE THRU 1500-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "VALIDATE"  PERFORM 2000-READ THRU 2000-EXIT
               WHEN "CANPAY"    PERFORM 2500-CANPAY THRU 2500-EXIT
               WHEN "HOLD"      PERFORM 3000-HOLD THRU 3000-EXIT
               WHEN "RELEASE"   PERFORM 3000-HOLD THRU 3000-EXIT
               WHEN "TERMCHK"   PERFORM 4000-TERMCHK THRU 4000-EXIT
               WHEN OTHER       PERFORM 9100-BAD-FUNC THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INIT SECTION.
       1000-START.
           MOVE "N" TO WS-FATAL-SW
           MOVE "N" TO WS-FOUND-SW
           MOVE "APSUPVAL" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "AP-001" TO CA-REQ-REF
           MOVE SPACES TO SV-NOTE
           MOVE "N" TO SV-CAN-PURCHASE SV-CAN-PAY SV-TERM-EXCEPTION
           MOVE 0 TO SV-DD-OUTSTANDING SV-DD-FAILED
           MOVE 0 TO SV-OPEN-ITEM-CNT
           MOVE FUNCTION UPPER-CASE(SV-SUPPLIER) TO SV-SUPPLIER
           MOVE FUNCTION UPPER-CASE(CA-COMPANY) TO CA-COMPANY.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1500-VALIDATE SECTION.
       1500-START.
           IF CA-COMPANY = SPACES OR CA-COMPANY = LOW-VALUES
               MOVE "APS0001" TO CA-REASON-CODE
               MOVE "A company is required for a supplier enquiry"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1500-EXIT
           END-IF
           IF SV-SUPPLIER = SPACES OR SV-SUPPLIER = LOW-VALUES
               MOVE "APS0002" TO CA-REASON-CODE
               MOVE "A supplier account code is required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> READ THE ACCOUNT AND WORK OUT WHAT MAY BE DONE WITH IT.
      *>----------------------------------------------------------------
       2000-READ SECTION.
       2000-START.
           PERFORM 2100-FETCH-SUPPLIER THRU 2100-EXIT
           IF WS-FATAL OR NOT WS-SUPPLIER-FOUND
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-FETCH-BALANCE THRU 2200-EXIT
           PERFORM 2300-FETCH-DILIGENCE THRU 2300-EXIT
           PERFORM 2400-DECIDE THRU 2400-EXIT.
       2000-EXIT.
           EXIT.

       2100-FETCH-SUPPLIER SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT LegalName, ISNULL(TradingName,''), "
                  "LegalForm, Status, BusinessSize, CurrencyCode, "
                  "AllowedCurrencies, PaymentMethod, VatLocation, "
                  "ISNULL(VatRegNo,''), CountryCode, SanctionsStatus, "
                  "CAST(IsCisOrConstructionDrc AS char(1)), "
                  "CAST(EInvoiceEnabled AS char(1)), "
                  "ISNULL(ApControlAccount,''), "
                  "ISNULL(DefaultExpenseAccount,''), "
                  "ISNULL(DimCostCentre,''), ISNULL(ApOwnerUser,''), "
                  "ISNULL(BuyerUser,''), ISNULL(TermsCode,''), "
                  "CAST(PurchaseHold AS char(1)), "
                  "CAST(PaymentHold AS char(1)), "
                  "ISNULL(HoldReason,''), "
                  "CAST(ISNULL(MinimumPaymentGBP,0) AS varchar(24)) "
                  "FROM ap.Supplier WHERE CompanyCode = ? "
                  "AND SupplierCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE SV-SUPPLIER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 24 TO SQLIO-COL-CNT
           MOVE 120 TO SQLIO-COL-WIDTH(1)
           MOVE 120 TO SQLIO-COL-WIDTH(2)
           MOVE 14 TO SQLIO-COL-WIDTH(3)
           MOVE 10 TO SQLIO-COL-WIDTH(4)
           MOVE 8 TO SQLIO-COL-WIDTH(5)
           MOVE 3 TO SQLIO-COL-WIDTH(6)
           MOVE 40 TO SQLIO-COL-WIDTH(7)
           MOVE 10 TO SQLIO-COL-WIDTH(8)
           MOVE 3 TO SQLIO-COL-WIDTH(9)
           MOVE 15 TO SQLIO-COL-WIDTH(10)
           MOVE 2 TO SQLIO-COL-WIDTH(11)
           MOVE 12 TO SQLIO-COL-WIDTH(12)
           MOVE 1 TO SQLIO-COL-WIDTH(13)
           MOVE 1 TO SQLIO-COL-WIDTH(14)
           MOVE 12 TO SQLIO-COL-WIDTH(15)
           MOVE 12 TO SQLIO-COL-WIDTH(16)
           MOVE 20 TO SQLIO-COL-WIDTH(17)
           MOVE 20 TO SQLIO-COL-WIDTH(18)
           MOVE 20 TO SQLIO-COL-WIDTH(19)
           MOVE 12 TO SQLIO-COL-WIDTH(20)
           MOVE 1 TO SQLIO-COL-WIDTH(21)
           MOVE 1 TO SQLIO-COL-WIDTH(22)
           MOVE 200 TO SQLIO-COL-WIDTH(23)
           MOVE 24 TO SQLIO-COL-WIDTH(24)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SUPPLIER-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE
               MOVE "APS0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Supplier " FUNCTION TRIM(SV-SUPPLIER)
                      " does not exist in company "
                      FUNCTION TRIM(CA-COMPANY)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 2100-EXIT
           END-IF
           MOVE "Y" TO WS-FOUND-SW
           PERFORM 2150-MOVE-FIELDS THRU 2150-EXIT.
       2100-EXIT.
           EXIT.

       2150-MOVE-FIELDS SECTION.
       2150-START.
           MOVE WS-R-LEGAL-NAME TO SV-LEGAL-NAME
           MOVE WS-R-TRADING-NAME TO SV-TRADING-NAME
           MOVE WS-R-LEGAL-FORM TO SV-LEGAL-FORM
           MOVE WS-R-STATUS TO SV-STATUS
           MOVE WS-R-SIZE TO SV-BUSINESS-SIZE
           MOVE WS-R-CURRENCY TO SV-CURRENCY
           MOVE WS-R-ALLOWED TO SV-ALLOWED-CCY
           MOVE WS-R-METHOD TO SV-PAY-METHOD
           MOVE WS-R-VAT-LOC TO SV-VAT-LOCATION
           MOVE WS-R-VAT-NO TO SV-VAT-REG-NO
           MOVE WS-R-COUNTRY TO SV-COUNTRY
           MOVE WS-R-SANCTIONS TO SV-SANCTIONS
           MOVE WS-R-CONTROL TO SV-AP-CONTROL
           MOVE WS-R-EXPENSE TO SV-EXPENSE-ACCOUNT
           MOVE WS-R-COSTCTR TO SV-DIM-COST-CENTRE
           MOVE WS-R-OWNER TO SV-OWNER-USER
           MOVE WS-R-BUYER TO SV-BUYER-USER
           MOVE WS-R-TERMS TO SV-TERMS-CODE
           MOVE SPACES TO SV-REASON
           MOVE WS-R-HOLD-REASON TO SV-REASON
           IF WS-R-CIS = "1"
               MOVE "Y" TO SV-CIS-FLAG
           ELSE
               MOVE "N" TO SV-CIS-FLAG
           END-IF
           IF WS-R-EINV = "1"
               MOVE "Y" TO SV-EINVOICE-FLAG
           ELSE
               MOVE "N" TO SV-EINVOICE-FLAG
           END-IF
           IF WS-R-PHOLD = "1"
               MOVE "Y" TO SV-PURCHASE-HOLD
           ELSE
               MOVE "N" TO SV-PURCHASE-HOLD
           END-IF
           IF WS-R-YHOLD = "1"
               MOVE "Y" TO SV-PAYMENT-HOLD
           ELSE
               MOVE "N" TO SV-PAYMENT-HOLD
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R-MINPAY)
           MOVE WS-NUM TO SV-MIN-PAYMENT.
       2150-EXIT.
           EXIT.

       2200-FETCH-BALANCE SECTION.
       2200-START.
           MOVE 0 TO SV-OPEN-BAL-GBP
           MOVE 0 TO SV-OPEN-ITEM-CNT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(SUM(ResidualAmountGBP),0) AS "
                  "varchar(30)), CAST(COUNT(*) AS varchar(12)) "
                  "FROM ap.OpenItem WHERE CompanyCode = ? "
                  "AND SupplierCode = ? "
                  "AND SettlementStatus IN ('OPEN','PARTIAL')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE SV-SUPPLIER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BALANCE-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2200-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 2200-EXIT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-B-RESIDUAL)
           MOVE WS-NUM TO SV-OPEN-BAL-GBP
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-B-COUNT)
           MOVE WS-NUM TO SV-OPEN-ITEM-CNT.
       2200-EXIT.
           EXIT.

       2300-FETCH-DILIGENCE SECTION.
       2300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(SUM(CASE WHEN Outcome IN "
                  "('PENDING','EXPIRED') THEN 1 ELSE 0 END) AS "
                  "varchar(12)), CAST(SUM(CASE WHEN Outcome = 'FAIL' "
                  "THEN 1 ELSE 0 END) AS varchar(12)) "
                  "FROM ap.SupplierDueDiligence WHERE CompanyCode = ? "
                  "AND SupplierCode = ? AND IsMandatory = 1"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE SV-SUPPLIER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-DD-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 2300-EXIT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-D-OUTSTANDING)
           MOVE WS-NUM TO SV-DD-OUTSTANDING
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-D-FAILED)
           MOVE WS-NUM TO SV-DD-FAILED.
       2300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE TWO POLICIES ARE DELIBERATELY SEPARATE (AP-001).  A
      *> PURCHASING SUSPENSION NEVER IMPLIES THAT THE EXISTING DEBT
      *> MAY NOT BE PAID, AND RETIREMENT NEVER BLOCKS SETTLEMENT OR
      *> HISTORICAL ENQUIRY (AP-024).
      *>----------------------------------------------------------------
       2400-DECIDE SECTION.
       2400-START.
           MOVE "Y" TO SV-CAN-PURCHASE
           MOVE "Y" TO SV-CAN-PAY
           MOVE SPACES TO SV-NOTE
           IF SV-PURCHASE-HOLD = "Y"
               MOVE "N" TO SV-CAN-PURCHASE
               MOVE SPACES TO SV-NOTE
               STRING "New purchasing is suspended; existing payables "
                      "remain due" DELIMITED BY SIZE
                   INTO SV-NOTE
           END-IF
           IF SV-PAYMENT-HOLD = "Y"
               MOVE "N" TO SV-CAN-PAY
               MOVE "Payment is on hold for this account"
                   TO SV-NOTE
           END-IF
           IF SV-ST-PENDING
               MOVE "N" TO SV-CAN-PURCHASE
               MOVE "N" TO SV-CAN-PAY
               MOVE SPACES TO SV-NOTE
               STRING "The account is pending approval and due "
                      "diligence" DELIMITED BY SIZE
                   INTO SV-NOTE
           END-IF
           IF SV-ST-RETIRED
               MOVE "N" TO SV-CAN-PURCHASE
               MOVE SPACES TO SV-NOTE
               STRING "Retired account; authorised settlement and "
                      "enquiry remain available" DELIMITED BY SIZE
                   INTO SV-NOTE
           END-IF
           IF SV-ST-SUSPENDED
               MOVE "N" TO SV-CAN-PURCHASE
           END-IF
           IF SV-SANCTIONS = "MATCH" OR SV-SANCTIONS = "BLOCKED"
               MOVE "N" TO SV-CAN-PURCHASE
               MOVE "N" TO SV-CAN-PAY
               MOVE "Sanctions screening blocks this account"
                   TO SV-NOTE
           END-IF
           IF SV-DD-FAILED > 0
               MOVE "N" TO SV-CAN-PURCHASE
           END-IF
           EVALUATE TRUE
               WHEN SV-CAN-PURCHASE = "N" AND SV-CAN-PAY = "N"
                   MOVE 6 TO CA-RETURN-CODE
                   MOVE "APS0006" TO CA-REASON-CODE
                   MOVE SV-NOTE TO CA-MESSAGE
                   ADD 1 TO CA-MSG-COUNT
               WHEN SV-CAN-PURCHASE = "N" OR SV-CAN-PAY = "N"
                   MOVE 4 TO CA-RETURN-CODE
                   MOVE "APS0007" TO CA-REASON-CODE
                   MOVE SV-NOTE TO CA-MESSAGE
                   ADD 1 TO CA-MSG-COUNT
               WHEN SV-DD-OUTSTANDING > 0
                   MOVE 4 TO CA-RETURN-CODE
                   MOVE "APS0008" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Due diligence evidence is outstanding for "
                          "this supplier" DELIMITED BY SIZE
                       INTO CA-MESSAGE
                   ADD 1 TO CA-MSG-COUNT
               WHEN OTHER
                   MOVE 0 TO CA-RETURN-CODE
           END-EVALUATE.
       2400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> CANPAY IS THE GATE USED BY THE PAYMENT PROPOSAL.  IT ANSWERS
      *> 06 (NEEDS AUTHORITY / HELD) RATHER THAN 08 SO THE OPERATOR IS
      *> TOLD WHAT WOULD RELEASE IT.
      *>----------------------------------------------------------------
       2500-CANPAY SECTION.
       2500-START.
           PERFORM 2000-READ THRU 2000-EXIT
           IF WS-FATAL OR NOT WS-SUPPLIER-FOUND
               GO TO 2500-EXIT
           END-IF
           IF SV-CAN-PAY = "Y"
               MOVE 0 TO CA-RETURN-CODE
               MOVE SPACES TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(SV-SUPPLIER)
                      " may be paid" DELIMITED BY SIZE
                   INTO CA-MESSAGE
           ELSE
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APS0009" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Payment to " FUNCTION TRIM(SV-SUPPLIER)
                      " is held: " FUNCTION TRIM(SV-NOTE)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       2500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3000-HOLD SECTION.
       3000-START.
           MOVE "AP.HOLD.SET" TO WS-PERM
           PERFORM 8000-CHECK-PERM THRU 8000-EXIT
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
           IF CA-FUNCTION = "HOLD" AND
              (SV-REASON = SPACES OR SV-REASON = LOW-VALUES)
               MOVE "APS0004" TO CA-REASON-CODE
               MOVE "A hold must state its reason" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           IF CA-FUNCTION = "HOLD"
               MOVE 1 TO WS-SET-FLAG
           ELSE
               MOVE 0 TO WS-SET-FLAG
           END-IF
           PERFORM 2000-READ THRU 2000-EXIT
           IF WS-FATAL OR NOT WS-SUPPLIER-FOUND
               GO TO 3000-EXIT
           END-IF
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE
           EVALUATE TRUE
               WHEN SV-HOLD-PURCH
                   MOVE "PurchaseHold" TO WS-HOLD-COL
                   PERFORM 3100-APPLY THRU 3100-EXIT
               WHEN SV-HOLD-PAY
                   MOVE "PaymentHold" TO WS-HOLD-COL
                   PERFORM 3100-APPLY THRU 3100-EXIT
               WHEN SV-HOLD-BOTH
                   MOVE "PurchaseHold" TO WS-HOLD-COL
                   PERFORM 3100-APPLY THRU 3100-EXIT
                   MOVE "PaymentHold" TO WS-HOLD-COL
                   PERFORM 3100-APPLY THRU 3100-EXIT
               WHEN OTHER
                   MOVE "APS0005" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "The hold type must be PURCHASE, PAYMENT or "
                          "BOTH" DELIMITED BY SIZE
                       INTO CA-MESSAGE
                   PERFORM 9000-REJECT THRU 9000-EXIT
           END-EVALUATE
           IF NOT WS-FATAL
               PERFORM 2000-READ THRU 2000-EXIT
               MOVE SPACES TO CA-MESSAGE
               IF WS-SET-FLAG = 1
                   STRING FUNCTION TRIM(SV-HOLD-TYPE)
                          " hold applied to "
                          FUNCTION TRIM(SV-SUPPLIER)
                       DELIMITED BY SIZE INTO CA-MESSAGE
               ELSE
                   STRING FUNCTION TRIM(SV-HOLD-TYPE)
                          " hold lifted for "
                          FUNCTION TRIM(SV-SUPPLIER)
                       DELIMITED BY SIZE INTO CA-MESSAGE
               END-IF
           END-IF.
       3000-EXIT.
           EXIT.

       3100-APPLY SECTION.
       3100-START.
           MOVE SPACES TO SQLIO-STMT
           IF WS-HOLD-COL = "PurchaseHold"
               STRING "UPDATE ap.Supplier SET PurchaseHold = ?, "
                      "HoldReason = ?, ModifiedBy = ?, "
                      "ModifiedAt = SYSDATETIME() "
                      "WHERE CompanyCode = ? AND SupplierCode = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
           ELSE
               STRING "UPDATE ap.Supplier SET PaymentHold = ?, "
                      "HoldReason = ?, ModifiedBy = ?, "
                      "ModifiedAt = SYSDATETIME() "
                      "WHERE CompanyCode = ? AND SupplierCode = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
           END-IF
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE WS-SET-FLAG TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           IF WS-SET-FLAG = 1
               MOVE SV-REASON TO SQLIO-PARM-VAL(2)
               MOVE "N" TO SQLIO-PARM-NULL(2)
           ELSE
               MOVE SPACES TO SQLIO-PARM-VAL(2)
               MOVE "Y" TO SQLIO-PARM-NULL(2)
           END-IF
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           MOVE SV-SUPPLIER TO SQLIO-PARM-VAL(5)
           MOVE "N" TO SQLIO-PARM-NULL(5)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SUPPLIER-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3100-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SUPPLIER-ROW
           PERFORM 3200-AUDIT THRU 3200-EXIT.
       3100-EXIT.
           EXIT.

       3200-AUDIT SECTION.
       3200-START.
           INITIALIZE WS-APSVC
           MOVE "AUDIT" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "ap.Supplier" TO SVC-AUD-ENTITY
           MOVE SV-SUPPLIER TO SVC-AUD-KEY
           IF WS-SET-FLAG = 1
               MOVE "HOLD" TO SVC-AUD-ACTION
               MOVE "0" TO SVC-AUD-OLD
               MOVE "1" TO SVC-AUD-NEW
           ELSE
               MOVE "RELEASE" TO SVC-AUD-ACTION
               MOVE "1" TO SVC-AUD-OLD
               MOVE "0" TO SVC-AUD-NEW
           END-IF
           MOVE WS-HOLD-COL TO SVC-AUD-FIELD
           MOVE SV-REASON TO SVC-AUD-REASON
           MOVE "N" TO SVC-AUD-SENS
           CALL "APSVC" USING WS-APSVC.
       3200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> D-25: THE MAXIMUM PAYMENT TERM IS A PARAMETER (BASELINE 60
      *> DAYS).  TERMS BEYOND IT NEED AN AUTHORISED EXCEPTION, AND A
      *> SMALL SUPPLIER IS REPORTED SEPARATELY BECAUSE THE POLICY FOR
      *> SMALL BUSINESSES IS STRICTER.
      *>----------------------------------------------------------------
       4000-TERMCHK SECTION.
       4000-START.
           PERFORM 2000-READ THRU 2000-EXIT
           IF WS-FATAL OR NOT WS-SUPPLIER-FOUND
               GO TO 4000-EXIT
           END-IF
           IF SV-TERMS-CODE = SPACES OR SV-TERMS-CODE = LOW-VALUES
               MOVE WS-R-TERMS TO SV-TERMS-CODE
           END-IF
           INITIALIZE WS-APSVC
           MOVE "PARMNUM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "TERMS.MAX.DAYS" TO SVC-TEXT-1
           MOVE 60 TO SVC-NUM-1
           CALL "APSVC" USING WS-APSVC
           MOVE SVC-NUM-OUT TO SV-MAX-TERM-DAYS
           INITIALIZE WS-APSVC
           MOVE "TERMS" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE SV-TERMS-CODE TO SVC-TEXT-1
           MOVE CA-BUSINESS-DATE TO SVC-DATE-1
           CALL "APSVC" USING WS-APSVC
           IF SVC-RC > 6
               MOVE 0 TO SV-TERM-DAYS
           ELSE
               MOVE SVC-INT-OUT TO SV-TERM-DAYS
           END-IF
           MOVE "N" TO SV-TERM-EXCEPTION
           IF SV-TERM-DAYS > SV-MAX-TERM-DAYS
               MOVE "Y" TO SV-TERM-EXCEPTION
               MOVE SV-TERM-DAYS TO WS-DAYS-ED
               MOVE SPACES TO CA-MESSAGE
               IF SV-SMALL
                   MOVE 6 TO CA-RETURN-CODE
                   MOVE "APS0010" TO CA-REASON-CODE
                   STRING "Agreed terms of "
                          FUNCTION TRIM(WS-DAYS-ED)
                          " days exceed the approved maximum for a "
                          "small supplier and need an authorised "
                          "exception" DELIMITED BY SIZE
                       INTO CA-MESSAGE
               ELSE
                   MOVE 4 TO CA-RETURN-CODE
                   MOVE "APS0011" TO CA-REASON-CODE
                   STRING "Agreed terms of "
                          FUNCTION TRIM(WS-DAYS-ED)
                          " days exceed the approved maximum payment "
                          "term" DELIMITED BY SIZE INTO CA-MESSAGE
               END-IF
               ADD 1 TO CA-MSG-COUNT
           ELSE
               MOVE 0 TO CA-RETURN-CODE
               MOVE SPACES TO CA-REASON-CODE
               MOVE "Agreed terms are within the approved maximum"
                   TO CA-MESSAGE
           END-IF.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8000-CHECK-PERM SECTION.
       8000-START.
           INITIALIZE WS-APSVC
           MOVE "PERM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE WS-PERM TO SVC-TEXT-1
           CALL "APSVC" USING WS-APSVC
           IF SVC-FLAG-1 NOT = "Y"
               MOVE "Y" TO WS-FATAL-SW
               MOVE SVC-RC TO CA-RETURN-CODE
               IF CA-RETURN-CODE < 6
                   MOVE 6 TO CA-RETURN-CODE
               END-IF
               MOVE "APS0012" TO CA-REASON-CODE
               MOVE SVC-MESSAGE TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       8000-EXIT.
           EXIT.

       8900-SVC-HEADER SECTION.
       8900-START.
           MOVE CA-COMPANY TO SVC-COMPANY
           MOVE CA-SITE TO SVC-SITE
           MOVE CA-USER TO SVC-USER
           MOVE CA-BUSINESS-DATE TO SVC-DATE
           MOVE CA-CORRELATION-ID TO SVC-CORRELATION.
       8900-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-FATAL-SW
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNC SECTION.
       9100-START.
           MOVE "APS0099" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "APSUPVAL does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE "Y" TO WS-FATAL-SW
           MOVE 12 TO CA-RETURN-CODE
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF CA-MESSAGE = SPACES
               MOVE "SQL failure in APSUPVAL" TO CA-MESSAGE
           END-IF
           ADD 1 TO CA-MSG-COUNT
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SUPPLIER-ROW.
       9300-EXIT.
           EXIT.
