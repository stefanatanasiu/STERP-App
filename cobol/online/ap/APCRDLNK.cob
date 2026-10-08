      *>================================================================
      *> PROGRAM-ID : APCRDLNK
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : SUPPLIER CREDITS LINKED TO PHYSICAL RETURNS, AND
      *>              SUPPLIER ADVANCES, REFUNDS AND AGREED LEGAL
      *>              SET-OFF.
      *>              A SUPPLIER CREDIT DOES NOT MOVE STOCK AND A
      *>              STOCK RETURN DOES NOT SETTLE AP (AP-010): THE
      *>              RETURN SITS IN SUPPLIER RETURN CLEARING UNTIL
      *>              THE CREDIT ARRIVES.
      *>              AN ADVANCE IS AN ASSET UNTIL IT IS VALIDLY
      *>              APPLIED, AND A DEBIT AP BALANCE NEVER BECOMES AN
      *>              AUTOMATIC NEGATIVE PAYMENT INSTRUCTION (AP-017).
      *> CALLED BY  : .NET REGISTER AND ALLOCATION FORMS, APCLOSE
      *> INTERFACE  : CALL "APCRDLNK" USING WS-APCRDLNK-CA
      *>              COPYBOOK APCRDLNKCA
      *> CALLS      : SQLIO, APSVC, APPOST
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1993-09-08 DJM  ORIGINAL SUPPLIER CREDIT AND DEPOSIT CONTROL
      *> 2001-05-30 KMP  CR1402 SET-OFF NEEDS A SIGNED AGREEMENT
      *> 2026-10-05 STERP RETURN CLEARING AND ADVANCE ASSET SPLIT
      *> 2026-10-07 STERP RETSTAT READ rl.ExpectedCreditGBP, WHICH IS
      *>                 HELD ON THE RETURN HEADER AND NOT ON THE
      *>                 LINE, SO EVERY RETURN LOOKED EMPTY.  THE LINE
      *>                 VALUE IN RETURN CLEARING IS ClearingValueGBP.
      *>================================================================
      *> REQ: AP-010, AP-017, AP-022
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APCRDLNK.
       AUTHOR. D J MILLER.
       DATE-WRITTEN. 08/09/1993.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY APSVCARE.
       COPY APPOSTWK.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
       01  WS-RET-ROW.
           05  WS-R-RETURNED           PIC X(24).
           05  WS-R-CREDITED           PIC X(24).
           05  WS-R-EXPECTED           PIC X(24).
           05  WS-R-ITEM               PIC X(40).
       01  WS-ADV-ROW.
           05  WS-A-ID                 PIC X(24).
           05  WS-A-SUPPLIER           PIC X(20).
           05  WS-A-CURRENCY           PIC X(3).
           05  WS-A-ORIGINAL           PIC X(24).
           05  WS-A-RESIDUAL           PIC X(24).
           05  WS-A-TYPE               PIC X(12).
       01  WS-ONE-COL.
           05  WS-C-VALUE              PIC X(40).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-NUM                  PIC S9(15)V9(6) VALUE 0.
           05  WS-MONEY-ED             PIC -(11)9.99.
           05  WS-QTY-ED               PIC -(9)9.999999.
       LINKAGE SECTION.
       COPY APCRDLNKCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "CRDLINK"  PERFORM 2000-CRDLINK THRU 2000-EXIT
               WHEN "RETSTAT"  PERFORM 2500-RETSTAT THRU 2500-EXIT
               WHEN "ADVANCE"  PERFORM 3000-ADVANCE THRU 3000-EXIT
               WHEN "APPLYADV" PERFORM 4000-APPLYADV THRU 4000-EXIT
               WHEN "REFUND"   PERFORM 5000-REFUND THRU 5000-EXIT
               WHEN "SETOFF"   PERFORM 6000-SETOFF THRU 6000-EXIT
               WHEN OTHER      PERFORM 9100-BAD-FUNC THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INIT SECTION.
       1000-START.
           MOVE "N" TO WS-FATAL-SW
           MOVE "APCRDLNK" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "AP-010" TO CA-REQ-REF
           MOVE SPACES TO CL-NOTE
           MOVE "N" TO CL-STOCK-AFFECTED
           MOVE FUNCTION UPPER-CASE(CA-COMPANY) TO CA-COMPANY
           IF CL-EFFECTIVE-DATE = 0
               MOVE CA-BUSINESS-DATE TO CL-EFFECTIVE-DATE
           END-IF
           IF CL-ADVANCE-CCY = SPACES
               MOVE "GBP" TO CL-ADVANCE-CCY
           END-IF
           IF CA-COMPANY = SPACES
               MOVE "APC0001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AP-010.  THE CREDIT IS LINKED TO THE RETURN, BUT THEY REMAIN
      *> SEPARATE EVENTS: THE CREDIT CHANGES AP AND TAX, THE RETURN
      *> CHANGED THE PHYSICAL QUANTITY.  THE REMAINING UNCREDITED
      *> RETURN VALUE STAYS VISIBLE IN SUPPLIER RETURN CLEARING.
      *>----------------------------------------------------------------
       2000-CRDLINK SECTION.
       2000-START.
           IF CL-RETURN-REF = SPACES
               MOVE "APC0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The supplier return reference is required to "
                      "link a credit" DELIMITED BY SIZE
                   INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           PERFORM 2500-RETSTAT THRU 2500-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           IF CL-CREDITED-GBP > CL-REMAINING-GBP
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APC0010" TO CA-REASON-CODE
               MOVE CL-REMAINING-GBP TO WS-MONEY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "The credit exceeds the remaining uncredited "
                      "return value of GBP "
                      FUNCTION TRIM(WS-MONEY-ED)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 2000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.PurchaseInvoice SET AttachmentRef = "
                  "COALESCE(AttachmentRef, ?), ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE CompanyCode = ? "
                  "AND ((? <> 0 AND PurchaseInvoiceId = ?) OR "
                  "(? <> '' AND RegisterNumber = ?)) AND "
                  "DocumentType = 'CREDITNOTE'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE SPACES TO SQLIO-PARM-VAL(1)
           STRING "Linked to supplier return "
                  FUNCTION TRIM(CL-RETURN-REF)
               DELIMITED BY SIZE INTO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE CL-CREDIT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE CL-CREDIT-REGISTER TO SQLIO-PARM-VAL(6)
           MOVE CL-CREDIT-REGISTER TO SQLIO-PARM-VAL(7)
           MOVE 7 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 7
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           COMPUTE CL-REMAINING-GBP =
               CL-REMAINING-GBP - CL-CREDITED-GBP
           MOVE CL-REMAINING-GBP TO CL-CLEARING-GBP
           MOVE "N" TO CL-STOCK-AFFECTED
           MOVE CL-REMAINING-GBP TO WS-MONEY-ED
           MOVE 4 TO CA-RETURN-CODE
           MOVE "APC0011" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Credit linked to the return. No stock moves; GBP "
                  FUNCTION TRIM(WS-MONEY-ED)
                  " remains in supplier return clearing"
               DELIMITED BY SIZE INTO CA-MESSAGE
           ADD 1 TO CA-MSG-COUNT.
       2000-EXIT.
           EXIT.

       2500-RETSTAT SECTION.
       2500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(SUM(rl.ReturnedQty),0) AS "
                  "varchar(24)), CAST(ISNULL(SUM(rl.CreditedQty),0) "
                  "AS varchar(24)), CAST(ISNULL(SUM("
                  "rl.ClearingValueGBP),0) AS varchar(24)), "
                  "ISNULL(MAX(rl.ItemCode),'') "
                  "FROM po.SupplierReturnLine rl JOIN "
                  "po.SupplierReturn r ON r.SupplierReturnId = "
                  "rl.SupplierReturnId WHERE r.CompanyCode = ? AND "
                  "r.ReturnRef = ? AND (? = 0 OR rl.LineNum = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE CL-RETURN-REF TO SQLIO-PARM-VAL(2)
           MOVE CL-RETURN-LINE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 40 TO SQLIO-COL-WIDTH(4)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-RET-ROW
           IF SQLIO-ERROR
      *>       THE PURCHASING MODULE MAY NOT BE DEPLOYED YET; TREAT
      *>       AN ABSENT RETURN TABLE AS NOTHING RETURNED RATHER THAN
      *>       A SYSTEM FAILURE.
               MOVE "CLOS" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-RET-ROW
               MOVE 0 TO CL-RETURNED-QTY CL-CREDITED-QTY
               MOVE 0 TO CL-REMAINING-QTY CL-EXPECTED-GBP
               MOVE 0 TO CL-REMAINING-GBP
               MOVE 4 TO CA-RETURN-CODE
               MOVE "APC0012" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No supplier return detail is available for "
                      FUNCTION TRIM(CL-RETURN-REF)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 2500-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 0 TO CL-RETURNED-QTY
               GO TO 2500-EXIT
           END-IF
           COMPUTE CL-RETURNED-QTY = FUNCTION NUMVAL(WS-R-RETURNED)
           COMPUTE CL-CREDITED-QTY = FUNCTION NUMVAL(WS-R-CREDITED)
           COMPUTE CL-REMAINING-QTY =
               CL-RETURNED-QTY - CL-CREDITED-QTY
           COMPUTE CL-EXPECTED-GBP = FUNCTION NUMVAL(WS-R-EXPECTED)
           COMPUTE CL-REMAINING-GBP =
               CL-EXPECTED-GBP - CL-CREDITED-GBP
           MOVE WS-R-ITEM TO CL-ITEM
           MOVE CL-REMAINING-QTY TO WS-QTY-ED
           MOVE SPACES TO CA-MESSAGE
           STRING FUNCTION TRIM(WS-QTY-ED)
                  " unit(s) returned but not yet credited"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AN ADVANCE IS A SUPPLIER PREPAYMENT ASSET AGAINST BANK, NOT A
      *> REDUCTION OF AP.  IT STAYS AN ASSET UNTIL A VALID INVOICE IS
      *> RECOGNISED AND THE ADVANCE IS APPLIED.
      *>----------------------------------------------------------------
       3000-ADVANCE SECTION.
       3000-START.
           IF CL-ADVANCE-AMT = 0
               MOVE "APC0003" TO CA-REASON-CODE
               MOVE "An advance amount is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           IF CL-ADVANCE-NUMBER = SPACES
               INITIALIZE WS-APSVC
               MOVE "NUMBER" TO SVC-FUNC
               PERFORM 8900-SVC-HEADER THRU 8900-EXIT
               MOVE "APPY" TO SVC-TEXT-1
               CALL "APSVC" USING WS-APSVC
               MOVE SVC-TEXT-OUT(1:30) TO CL-ADVANCE-NUMBER
           END-IF
           IF CL-ADVANCE-GBP = 0
               MOVE CL-ADVANCE-AMT TO CL-ADVANCE-GBP
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; "
                  "INSERT ap.OpenItem (CompanyCode, SupplierCode, "
                  "ItemType, DocumentNumber, DocumentDate, DueDate, "
                  "PostingDate, CurrencyCode, FxRate, OriginalAmount,"
                  " OriginalAmountGBP, ResidualAmount, "
                  "ResidualAmountGBP, SettlementStatus, CreatedBy) "
                  "SELECT ?, ?, 'ADVANCE', ?, CAST(? AS date), "
                  "CAST(? AS date), CAST(? AS date), ?, 1, "
                  "-CAST(? AS decimal(18,2)), "
                  "-CAST(? AS decimal(18,2)), "
                  "-CAST(? AS decimal(18,2)), "
                  "-CAST(? AS decimal(18,2)), 'OPEN', ? "
                  "WHERE NOT EXISTS (SELECT 1 FROM ap.OpenItem o "
                  "WHERE o.CompanyCode = ? AND o.ItemType = 'ADVANCE'"
                  " AND o.DocumentNumber = ?); "
                  "SELECT CAST(SCOPE_IDENTITY() AS varchar(24))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE CL-SUPPLIER TO SQLIO-PARM-VAL(2)
           MOVE CL-ADVANCE-NUMBER TO SQLIO-PARM-VAL(3)
           MOVE CL-EFFECTIVE-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE CL-ADVANCE-CCY TO SQLIO-PARM-VAL(7)
           MOVE CL-ADVANCE-AMT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE CL-ADVANCE-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE CL-ADVANCE-AMT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE CL-ADVANCE-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE CA-USER TO SQLIO-PARM-VAL(12)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(13)
           MOVE CL-ADVANCE-NUMBER TO SQLIO-PARM-VAL(14)
           MOVE 14 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 14
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3000-EXIT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-C-VALUE)
           MOVE WS-NUM TO CL-ADVANCE-ID
           MOVE CL-ADVANCE-GBP TO CL-ADVANCE-RESID
           PERFORM 3100-POST-ADVANCE THRU 3100-EXIT
           MOVE CL-ADVANCE-GBP TO WS-MONEY-ED
           MOVE 4 TO CA-RETURN-CODE
           MOVE "APC0013" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Advance " FUNCTION TRIM(CL-ADVANCE-NUMBER)
                  " of GBP " FUNCTION TRIM(WS-MONEY-ED)
                  " recorded as a supplier prepayment asset"
               DELIMITED BY SIZE INTO CA-MESSAGE
           ADD 1 TO CA-MSG-COUNT.
       3000-EXIT.
           EXIT.

       3100-POST-ADVANCE SECTION.
       3100-START.
           IF CA-CORRELATION-ID = SPACES
               GO TO 3100-EXIT
           END-IF
           INITIALIZE WS-APPOST
           MOVE "POST" TO PJ-FUNC
           MOVE CA-COMPANY TO PJ-COMPANY
           MOVE CA-SITE TO PJ-SITE
           MOVE CA-USER TO PJ-USER
           MOVE CA-BUSINESS-DATE TO PJ-BUSINESS-DATE
           MOVE CA-CORRELATION-ID TO PJ-CORRELATION
           MOVE "AP.ADVANCE" TO PJ-EVENT-CODE
           MOVE "AP" TO PJ-JOURNAL-TYPE
           MOVE CL-ADVANCE-NUMBER TO PJ-SOURCE-REF
           MOVE SPACES TO PJ-DESCRIPTION
           STRING "Supplier advance " FUNCTION TRIM(CL-SUPPLIER)
               DELIMITED BY SIZE INTO PJ-DESCRIPTION
           MOVE CL-EFFECTIVE-DATE TO PJ-POSTING-DATE
           MOVE CL-EFFECTIVE-DATE TO PJ-DOCUMENT-DATE
           MOVE "GBP" TO PJ-CURRENCY
           MOVE 1 TO PJ-FX-RATE
           MOVE "JNL" TO PJ-SERIES-CODE
           MOVE "AP" TO PJ-SUBLEDGER-TYPE
           MOVE CL-SUPPLIER TO PJ-SUBLEDGER-REF
           MOVE 2 TO PJ-LINE-CNT
           MOVE "1270" TO PJ-L-ACCOUNT(1)
           MOVE CL-ADVANCE-GBP TO PJ-L-DR(1)
           MOVE "Deposits paid to suppliers" TO PJ-L-NARRATIVE(1)
           MOVE "1300" TO PJ-L-ACCOUNT(2)
           MOVE CL-ADVANCE-GBP TO PJ-L-CR(2)
           MOVE "Bank current account" TO PJ-L-NARRATIVE(2)
           CALL "APPOST" USING WS-APPOST.
       3100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> APPLYING AN ADVANCE DEBITS AP AND CREDITS THE ADVANCE ASSET.
      *> IT CANNOT BE APPLIED BEYOND ITS RESIDUAL, SO IT CLEARS ONCE.
      *>----------------------------------------------------------------
       4000-APPLYADV SECTION.
       4000-START.
           PERFORM 7000-READ-ADVANCE THRU 7000-EXIT
           IF WS-FATAL
               GO TO 4000-EXIT
           END-IF
           IF CL-APPLY-AMT = 0
               MOVE CL-ADVANCE-RESID TO CL-APPLY-AMT
           END-IF
           IF CL-APPLY-AMT > CL-ADVANCE-RESID
               MOVE CL-ADVANCE-RESID TO CL-APPLY-AMT
               MOVE 4 TO CA-RETURN-CODE
               MOVE "APC0014" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The application was capped at the remaining "
                      "advance; an advance clears exactly once"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF
           IF CL-APPLY-AMT <= 0
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APC0015" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The advance has already been fully applied"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 4000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ap.Allocation (CompanyCode, SupplierCode, "
                  "SourceType, SourceOpenItemId, TargetOpenItemId, "
                  "EffectiveDate, CurrencyCode, AppliedAmount, "
                  "AppliedAmountGBP, CreatedBy) VALUES (?, ?, "
                  "'ADVANCE', ?, ?, CAST(? AS date), ?, ?, ?, ?); "
                  "UPDATE ap.OpenItem SET ResidualAmount = "
                  "ResidualAmount + CAST(? AS decimal(18,2)), "
                  "ResidualAmountGBP = ResidualAmountGBP + "
                  "CAST(? AS decimal(18,2)), SettlementStatus = CASE "
                  "WHEN ResidualAmountGBP + CAST(? AS decimal(18,2)) "
                  ">= 0 THEN 'SETTLED' ELSE 'PARTIAL' END "
                  "WHERE OpenItemId = ?; "
                  "UPDATE ap.OpenItem SET ResidualAmount = "
                  "ResidualAmount - CAST(? AS decimal(18,2)), "
                  "ResidualAmountGBP = ResidualAmountGBP - "
                  "CAST(? AS decimal(18,2)), AdvanceAppliedGBP = "
                  "AdvanceAppliedGBP + CAST(? AS decimal(18,2)), "
                  "SettlementStatus = CASE WHEN ResidualAmountGBP - "
                  "CAST(? AS decimal(18,2)) <= 0 THEN 'SETTLED' ELSE "
                  "'PARTIAL' END WHERE OpenItemId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE CL-SUPPLIER TO SQLIO-PARM-VAL(2)
           MOVE CL-ADVANCE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE CL-TARGET-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE CL-EFFECTIVE-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE CL-ADVANCE-CCY TO SQLIO-PARM-VAL(6)
           MOVE CL-APPLY-AMT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE CA-USER TO SQLIO-PARM-VAL(9)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
           MOVE CL-ADVANCE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(13)
           MOVE CL-APPLY-AMT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(14)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(15)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(16)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(17)
           MOVE CL-TARGET-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(18)
           MOVE 18 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 18
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-FATAL
               GO TO 4000-EXIT
           END-IF
           COMPUTE CL-ADVANCE-RESID =
               CL-ADVANCE-RESID - CL-APPLY-AMT
           PERFORM 4100-POST-APPLY THRU 4100-EXIT
           MOVE CL-APPLY-AMT TO WS-MONEY-ED
           IF CA-RETURN-CODE = 0
               MOVE SPACES TO CA-MESSAGE
               STRING "GBP " FUNCTION TRIM(WS-MONEY-ED)
                      " of advance applied; the expense is not "
                      "recorded twice" DELIMITED BY SIZE
                   INTO CA-MESSAGE
           END-IF.
       4000-EXIT.
           EXIT.

       4100-POST-APPLY SECTION.
       4100-START.
           IF CA-CORRELATION-ID = SPACES
               GO TO 4100-EXIT
           END-IF
           INITIALIZE WS-APPOST
           MOVE "POST" TO PJ-FUNC
           MOVE CA-COMPANY TO PJ-COMPANY
           MOVE CA-SITE TO PJ-SITE
           MOVE CA-USER TO PJ-USER
           MOVE CA-BUSINESS-DATE TO PJ-BUSINESS-DATE
           MOVE SPACES TO PJ-CORRELATION
           STRING FUNCTION TRIM(CA-CORRELATION-ID) "-ADV"
               DELIMITED BY SIZE INTO PJ-CORRELATION
           MOVE "AP.ADVAPPLY" TO PJ-EVENT-CODE
           MOVE "AP" TO PJ-JOURNAL-TYPE
           MOVE CL-ADVANCE-NUMBER TO PJ-SOURCE-REF
           MOVE SPACES TO PJ-DESCRIPTION
           STRING "Application of supplier advance "
                  FUNCTION TRIM(CL-ADVANCE-NUMBER)
               DELIMITED BY SIZE INTO PJ-DESCRIPTION
           MOVE CL-EFFECTIVE-DATE TO PJ-POSTING-DATE
           MOVE CL-EFFECTIVE-DATE TO PJ-DOCUMENT-DATE
           MOVE "GBP" TO PJ-CURRENCY
           MOVE 1 TO PJ-FX-RATE
           MOVE "JNL" TO PJ-SERIES-CODE
           MOVE "AP" TO PJ-SUBLEDGER-TYPE
           MOVE CL-SUPPLIER TO PJ-SUBLEDGER-REF
           MOVE 2 TO PJ-LINE-CNT
           MOVE "2000" TO PJ-L-ACCOUNT(1)
           MOVE CL-APPLY-AMT TO PJ-L-DR(1)
           MOVE "Trade creditors control" TO PJ-L-NARRATIVE(1)
           MOVE "1270" TO PJ-L-ACCOUNT(2)
           MOVE CL-APPLY-AMT TO PJ-L-CR(2)
           MOVE "Supplier advance applied" TO PJ-L-NARRATIVE(2)
           CALL "APPOST" USING WS-APPOST
           IF PJ-RC < 8
               MOVE PJ-JOURNAL-NUMBER TO CL-NOTE
           END-IF.
       4100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> A REFUND OF AN UNAPPLIED ADVANCE REDUCES THE ADVANCE ASSET.
      *> A REFUND OF A DEBIT AP BALANCE REDUCES THAT AP BALANCE.  THEY
      *> ARE DISTINCT CASES AND ARE NOT INTERCHANGEABLE (AP-017).
      *>----------------------------------------------------------------
       5000-REFUND SECTION.
       5000-START.
           IF NOT (CL-RF-ADVANCE OR CL-RF-DEBITBAL)
               MOVE "APC0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The refund type must be ADVANCE or DEBITBAL"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           PERFORM 7000-READ-ADVANCE THRU 7000-EXIT
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
           IF CL-REFUND-AMT > CL-ADVANCE-RESID
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APC0016" TO CA-REASON-CODE
               MOVE CL-ADVANCE-RESID TO WS-MONEY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "The refund exceeds the unapplied balance of "
                      "GBP " FUNCTION TRIM(WS-MONEY-ED)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.OpenItem SET ResidualAmount = "
                  "ResidualAmount + CAST(? AS decimal(18,2)), "
                  "ResidualAmountGBP = ResidualAmountGBP + "
                  "CAST(? AS decimal(18,2)), SettlementStatus = CASE "
                  "WHEN ResidualAmountGBP + CAST(? AS decimal(18,2)) "
                  ">= 0 THEN 'SETTLED' ELSE 'PARTIAL' END, "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE OpenItemId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CL-REFUND-AMT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE CA-USER TO SQLIO-PARM-VAL(4)
           MOVE CL-ADVANCE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE 5 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
           MOVE CL-REFUND-AMT TO WS-MONEY-ED
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-MESSAGE
           IF CL-RF-ADVANCE
               STRING "GBP " FUNCTION TRIM(WS-MONEY-ED)
                      " refunded against the unapplied advance asset"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               STRING "GBP " FUNCTION TRIM(WS-MONEY-ED)
                      " refunded against the debit payable balance"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> A LEGAL SET-OFF AGAINST A CUSTOMER BALANCE NEEDS A RECORDED
      *> AGREEMENT AND AN APPROVER.  IT IS NEVER APPLIED SILENTLY.
      *>----------------------------------------------------------------
       6000-SETOFF SECTION.
       6000-START.
           IF CL-SETOFF-CUSTOMER = SPACES OR CL-SETOFF-AMT = 0
               MOVE "APC0005" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A set-off needs the customer account and the "
                      "amount" DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 6000-EXIT
           END-IF
           IF CL-SETOFF-AGREED NOT = "Y"
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APC0017" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A set-off may only be applied where a legally "
                      "agreed right of set-off is recorded"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 6000-EXIT
           END-IF
           INITIALIZE WS-APSVC
           MOVE "PERM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "AP.PAYMENT.APPROVE" TO SVC-TEXT-1
           CALL "APSVC" USING WS-APSVC
           IF SVC-FLAG-1 NOT = "Y"
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APC0018" TO CA-REASON-CODE
               MOVE SVC-MESSAGE TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 6000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.OpenItem SET SetOffAgainstCustomer = ?, "
                  "SetOffApprovalId = NULLIF(?,0), ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE CompanyCode = ? "
                  "AND OpenItemId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CL-SETOFF-CUSTOMER TO SQLIO-PARM-VAL(1)
           MOVE CL-APPROVAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(4)
           MOVE CL-TARGET-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-FATAL
               GO TO 6000-EXIT
           END-IF
           MOVE CL-SETOFF-AMT TO WS-MONEY-ED
           MOVE 4 TO CA-RETURN-CODE
           MOVE "APC0019" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Set-off of GBP " FUNCTION TRIM(WS-MONEY-ED)
                  " recorded against customer "
                  FUNCTION TRIM(CL-SETOFF-CUSTOMER)
                  " under the recorded agreement"
               DELIMITED BY SIZE INTO CA-MESSAGE
           ADD 1 TO CA-MSG-COUNT.
       6000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       7000-READ-ADVANCE SECTION.
       7000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CAST(OpenItemId AS varchar(24)), "
                  "SupplierCode, CurrencyCode, "
                  "CAST(OriginalAmountGBP AS varchar(24)), "
                  "CAST(ResidualAmountGBP AS varchar(24)), ItemType "
                  "FROM ap.OpenItem WHERE CompanyCode = ? AND "
                  "((? <> 0 AND OpenItemId = ?) OR (? <> '' AND "
                  "DocumentNumber = ?))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE CL-ADVANCE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE CL-ADVANCE-NUMBER TO SQLIO-PARM-VAL(4)
           MOVE CL-ADVANCE-NUMBER TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 3 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 24 TO SQLIO-COL-WIDTH(5)
           MOVE 12 TO SQLIO-COL-WIDTH(6)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ADV-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 7000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE
               MOVE "APC0006" TO CA-REASON-CODE
               MOVE "That supplier advance was not found"
                   TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 7000-EXIT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-A-ID)
           MOVE WS-NUM TO CL-ADVANCE-ID
           MOVE WS-A-SUPPLIER TO CL-SUPPLIER
           MOVE WS-A-CURRENCY TO CL-ADVANCE-CCY
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-A-ORIGINAL)
           COMPUTE CL-ADVANCE-GBP = 0 - WS-NUM
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-A-RESIDUAL)
           COMPUTE CL-ADVANCE-RESID = 0 - WS-NUM.
       7000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8500-EXEC-UPDATE SECTION.
       8500-START.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 8500-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       8500-EXIT.
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
           MOVE "APC0099" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "APCRDLNK does not support function "
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
               MOVE "SQL failure in APCRDLNK" TO CA-MESSAGE
           END-IF
           ADD 1 TO CA-MSG-COUNT
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       9300-EXIT.
           EXIT.
