      *>================================================================
      *> PROGRAM-ID : APBLOCK
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : MATCH DISPUTES, PAYMENT BLOCKS AND THE
      *>              CONTRACTUAL DUE AND DISCOUNT DATES.
      *>              A PAYMENT BLOCK IS NOT AN ACCOUNTING STATE: A
      *>              POSTED LIABILITY CAN STAY BLOCKED FOR PAYMENT
      *>              AND AN ACCOUNTING EXCEPTION IS NEVER TREATED AS
      *>              A CLEAN MATCH (AP-008).  EVERY BLOCK AND DISPUTE
      *>              CARRIES AMOUNT, REASON, OWNER AND RESOLUTION
      *>              THROUGH core.ExceptionItem.
      *>              LATE INTERNAL PROCESSING DOES NOT RESTART THE
      *>              SUPPLIER'S CONTRACTUAL DUE DATE CLOCK (AP-009).
      *> CALLED BY  : .NET MATCHING AND REGISTER FORMS, APMATCH
      *>              CALLERS, APCLOSE
      *> INTERFACE  : CALL "APBLOCK" USING WS-APBLOCK-CA
      *>              COPYBOOK APBLOCKCA
      *> CALLS      : SQLIO, APSVC
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1992-01-30 DJM  ORIGINAL PAYMENT SUSPENSE AND QUERY CONTROL
      *> 2000-06-14 KMP  CR1102 QUERY OWNER AND TARGET RESOLUTION DATE
      *> 2026-10-05 STERP core.ExceptionItem, PENDING MATCH CLEARING
      *>================================================================
      *> REQ: AP-008, AP-009, AP-022, D-22, D-25
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APBLOCK.
       AUTHOR. D J MILLER.
       DATE-WRITTEN. 30/01/1992.
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
       01  WS-INV-ROW.
           05  WS-R-ID                 PIC X(24).
           05  WS-R-SUPPLIER           PIC X(20).
           05  WS-R-TERMS              PIC X(12).
           05  WS-R-DOCDATE            PIC X(8).
           05  WS-R-RECEIVED           PIC X(8).
           05  WS-R-GROSS              PIC X(24).
           05  WS-R-OWNER              PIC X(20).
           05  WS-R-ACCTSTATE          PIC X(14).
           05  WS-R-PAYELIG            PIC X(14).
       01  WS-ONE-COL.
           05  WS-C-VALUE              PIC X(40).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-NUM                  PIC S9(15)V9(6) VALUE 0.
           05  WS-FLAG                 PIC 9 VALUE 0.
           05  WS-MONEY-ED             PIC -(11)9.99.
       LINKAGE SECTION.
       COPY APBLOCKCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "BLOCK"   PERFORM 2000-BLOCK THRU 2000-EXIT
               WHEN "RELEASE" PERFORM 2000-BLOCK THRU 2000-EXIT
               WHEN "DISPUTE" PERFORM 3000-DISPUTE THRU 3000-EXIT
               WHEN "DUEDATE" PERFORM 4000-DUEDATE THRU 4000-EXIT
               WHEN "PENDING" PERFORM 5000-PENDING THRU 5000-EXIT
               WHEN OTHER     PERFORM 9100-BAD-FUNC THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INIT SECTION.
       1000-START.
           MOVE "N" TO WS-FATAL-SW
           MOVE "APBLOCK" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "AP-008" TO CA-REQ-REF
           MOVE SPACES TO BK-NOTE
           MOVE 0 TO BK-ITEMS-AFFECTED
           MOVE FUNCTION UPPER-CASE(CA-COMPANY) TO CA-COMPANY
           IF BK-ACTION = SPACES OR BK-ACTION = LOW-VALUES
               IF CA-FUNCTION = "RELEASE"
                   MOVE "CLEAR" TO BK-ACTION
               ELSE
                   MOVE "SET" TO BK-ACTION
               END-IF
           END-IF
           IF CA-FUNCTION = "RELEASE"
               MOVE "CLEAR" TO BK-ACTION
           END-IF
           IF CA-COMPANY = SPACES
               MOVE "APK0001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> A BLOCK IS A PAYMENT DECISION.  IT NEVER TOUCHES THE
      *> ACCOUNTING STATE, SO AN INVOICE CAN BE POSTED AND STILL BE
      *> UNPAYABLE, WHICH IS EXACTLY WHAT AP-008 REQUIRES.
      *>----------------------------------------------------------------
       2000-BLOCK SECTION.
       2000-START.
           INITIALIZE WS-APSVC
           MOVE "PERM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "AP.HOLD.SET" TO SVC-TEXT-1
           CALL "APSVC" USING WS-APSVC
           IF SVC-FLAG-1 NOT = "Y"
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APK0002" TO CA-REASON-CODE
               MOVE SVC-MESSAGE TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 2000-EXIT
           END-IF
           IF BK-ACT-SET AND
              (BK-REASON = SPACES OR BK-REASON = LOW-VALUES)
               MOVE "APK0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A payment block must state its reason, amount "
                      "and owner" DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF BK-ACT-CLEAR AND
              (BK-RESOLUTION = SPACES OR BK-RESOLUTION = LOW-VALUES)
               MOVE "APK0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Releasing a payment block needs a recorded "
                      "resolution" DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           PERFORM 7000-READ-INVOICE THRU 7000-EXIT
           IF BK-ACT-SET
               MOVE 1 TO WS-FLAG
               MOVE "Y" TO BK-IS-BLOCKED
               MOVE "BLOCKED" TO BK-PAY-ELIGIBILITY
           ELSE
               MOVE 0 TO WS-FLAG
               MOVE "N" TO BK-IS-BLOCKED
               MOVE "APPROVED" TO BK-PAY-ELIGIBILITY
           END-IF
           PERFORM 2100-UPDATE-OPEN-ITEMS THRU 2100-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-UPDATE-INVOICE THRU 2200-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           IF BK-ACT-SET
               PERFORM 2300-RAISE-EXCEPTION THRU 2300-EXIT
           END-IF
           PERFORM 2400-AUDIT THRU 2400-EXIT
           MOVE SPACES TO CA-MESSAGE
           IF BK-ACT-SET
               MOVE 4 TO CA-RETURN-CODE
               MOVE "APK0010" TO CA-REASON-CODE
               STRING "Payment blocked; the liability remains "
                      "recognised and reportable"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           ELSE
               MOVE 0 TO CA-RETURN-CODE
               STRING "Payment block released"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       2000-EXIT.
           EXIT.

       2100-UPDATE-OPEN-ITEMS SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.OpenItem SET IsBlocked = ?, BlockReason "
                  "= CASE WHEN ? = 1 THEN ? ELSE NULL END, "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE CompanyCode = ? AND ((? <> 0 AND "
                  "OpenItemId = ?) OR (? <> 0 AND "
                  "PurchaseInvoiceId = ?) OR (? <> '' AND "
                  "DocumentNumber = ?))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-FLAG TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE BK-REASON TO SQLIO-PARM-VAL(3)
           MOVE CA-USER TO SQLIO-PARM-VAL(4)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(5)
           MOVE BK-OPEN-ITEM-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE BK-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE BK-DOCUMENT-NUMBER TO SQLIO-PARM-VAL(10)
           MOVE BK-DOCUMENT-NUMBER TO SQLIO-PARM-VAL(11)
           MOVE 11 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 11
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           MOVE SQLIO-ROWCOUNT TO BK-ITEMS-AFFECTED.
       2100-EXIT.
           EXIT.

       2200-UPDATE-INVOICE SECTION.
       2200-START.
           IF BK-INVOICE-ID = 0 AND BK-REGISTER-NUMBER = SPACES
               GO TO 2200-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.PurchaseInvoice SET PaymentEligibility "
                  "= ?, BlockReason = CASE WHEN ? = 1 THEN ? ELSE "
                  "NULL END, ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() WHERE CompanyCode = ? AND "
                  "((? <> 0 AND PurchaseInvoiceId = ?) OR "
                  "(? <> '' AND RegisterNumber = ?))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE BK-PAY-ELIGIBILITY TO SQLIO-PARM-VAL(1)
           MOVE WS-FLAG TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE BK-REASON TO SQLIO-PARM-VAL(3)
           MOVE CA-USER TO SQLIO-PARM-VAL(4)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(5)
           MOVE BK-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE BK-REGISTER-NUMBER TO SQLIO-PARM-VAL(8)
           MOVE BK-REGISTER-NUMBER TO SQLIO-PARM-VAL(9)
           MOVE 9 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       2200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> COM-022: AN EXCEPTION WITHOUT AN OWNER NEVER GETS WORKED, SO
      *> THE AP OWNER OF THE DOCUMENT IS USED WHEN THE CALLER DID NOT
      *> NAME ONE.
      *>----------------------------------------------------------------
       2300-RAISE-EXCEPTION SECTION.
       2300-START.
           INITIALIZE WS-APSVC
           MOVE "EXCEPT" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           IF BK-SEVERITY = SPACES
               MOVE "HOLD" TO SVC-EXC-SEVERITY
           ELSE
               MOVE BK-SEVERITY TO SVC-EXC-SEVERITY
           END-IF
           IF BK-CATEGORY = SPACES
               MOVE "PAYMENT BLOCK" TO SVC-EXC-CATEGORY
           ELSE
               MOVE BK-CATEGORY TO SVC-EXC-CATEGORY
           END-IF
           MOVE "PURCHINVOICE" TO SVC-EXC-DOCTYPE
           IF BK-REGISTER-NUMBER NOT = SPACES
               MOVE BK-REGISTER-NUMBER TO SVC-EXC-DOCREF
           ELSE
               MOVE BK-DOCUMENT-NUMBER TO SVC-EXC-DOCREF
           END-IF
           IF BK-OWNER-USER NOT = SPACES
               MOVE BK-OWNER-USER TO SVC-EXC-OWNER
           ELSE
               MOVE WS-R-OWNER TO SVC-EXC-OWNER
           END-IF
           IF BK-OWNER-ROLE NOT = SPACES
               MOVE BK-OWNER-ROLE TO SVC-EXC-ROLE
           ELSE
               MOVE "APMGR" TO SVC-EXC-ROLE
           END-IF
           MOVE BK-AMOUNT-GBP TO SVC-EXC-AMOUNT
           MOVE BK-REASON TO SVC-EXC-MESSAGE
           CALL "APSVC" USING WS-APSVC
           MOVE SVC-EXC-ID TO BK-EXCEPTION-ID.
       2300-EXIT.
           EXIT.

       2400-AUDIT SECTION.
       2400-START.
           INITIALIZE WS-APSVC
           MOVE "AUDIT" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "ap.OpenItem" TO SVC-AUD-ENTITY
           IF BK-REGISTER-NUMBER NOT = SPACES
               MOVE BK-REGISTER-NUMBER TO SVC-AUD-KEY
           ELSE
               MOVE BK-DOCUMENT-NUMBER TO SVC-AUD-KEY
           END-IF
           IF BK-ACT-SET
               MOVE "BLOCK" TO SVC-AUD-ACTION
               MOVE "0" TO SVC-AUD-OLD
               MOVE "1" TO SVC-AUD-NEW
               MOVE BK-REASON TO SVC-AUD-REASON
           ELSE
               MOVE "UNBLOCK" TO SVC-AUD-ACTION
               MOVE "1" TO SVC-AUD-OLD
               MOVE "0" TO SVC-AUD-NEW
               MOVE BK-RESOLUTION TO SVC-AUD-REASON
           END-IF
           MOVE "IsBlocked" TO SVC-AUD-FIELD
           MOVE "N" TO SVC-AUD-SENS
           CALL "APSVC" USING WS-APSVC.
       2400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> A MATCH DISPUTE IS RECORDED AGAINST THE MATCH ROW AND THE OPEN
      *> ITEM SEPARATELY FROM THE ACCOUNTING STATE, AND ALWAYS WITH AN
      *> OWNER.
      *>----------------------------------------------------------------
       3000-DISPUTE SECTION.
       3000-START.
           IF BK-ACT-SET AND BK-REASON = SPACES
               MOVE "APK0005" TO CA-REASON-CODE
               MOVE "A dispute must state its reason" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           PERFORM 7000-READ-INVOICE THRU 7000-EXIT
           IF BK-ACT-SET
               MOVE 1 TO WS-FLAG
               MOVE "Y" TO BK-IS-DISPUTED
           ELSE
               MOVE 0 TO WS-FLAG
               MOVE "N" TO BK-IS-DISPUTED
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.InvoiceMatch SET Status = CASE WHEN ? = "
                  "1 THEN 'DISPUTED' ELSE 'PROPOSED' END, "
                  "ToleranceNote = COALESCE(NULLIF(?,''), "
                  "ToleranceNote) WHERE CompanyCode = ? AND "
                  "PurchaseInvoiceId = ? AND Status <> 'REVERSED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-FLAG TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           IF BK-ACT-SET
               MOVE BK-REASON TO SQLIO-PARM-VAL(2)
           ELSE
               MOVE BK-RESOLUTION TO SQLIO-PARM-VAL(2)
           END-IF
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE BK-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE 4 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.OpenItem SET IsDisputed = ?, "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE CompanyCode = ? AND PurchaseInvoiceId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-FLAG TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE BK-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE 4 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF BK-ACT-SET
               MOVE "MATCH DISPUTE" TO BK-CATEGORY
               PERFORM 2300-RAISE-EXCEPTION THRU 2300-EXIT
               MOVE 4 TO CA-RETURN-CODE
               MOVE "APK0011" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Match dispute recorded and owned; the "
                      "accounting state is unchanged"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           ELSE
               MOVE 0 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Match dispute resolved"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AP-009.  THE BASIS IS ALWAYS THE SUPPLIER'S DOCUMENT DATE.
      *> AN AUTHORISED OVERRIDE IS ALLOWED BUT MUST STATE ITS REASON,
      *> AND THE UNADJUSTED DATE IS KEPT VISIBLE.
      *>----------------------------------------------------------------
       4000-DUEDATE SECTION.
       4000-START.
           IF BK-DOCUMENT-DATE = 0
               MOVE "APK0006" TO CA-REASON-CODE
               MOVE "A document date is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           INITIALIZE WS-APSVC
           MOVE "TERMS" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE BK-TERMS-CODE TO SVC-TEXT-1
           MOVE BK-DOCUMENT-DATE TO SVC-DATE-1
           MOVE BK-AMOUNT-GBP TO SVC-NUM-1
           CALL "APSVC" USING WS-APSVC
           MOVE SVC-DATE-OUT TO BK-DUE-DATE
           MOVE SVC-DATE-2 TO BK-DUE-DATE-RAW
           MOVE SVC-DATE-OUT2 TO BK-DISCOUNT-DATE
           MOVE SVC-NUM-OUT TO BK-DISCOUNT-AMT
           MOVE SVC-INT-OUT TO BK-TERM-DAYS
           MOVE SVC-TEXT-OUT(1:20) TO BK-TERMS-SOURCE
           MOVE SVC-RC TO CA-RETURN-CODE
           MOVE SVC-REASON TO CA-REASON-CODE
           IF BK-DUE-DATE-RAW = 0
               MOVE BK-DUE-DATE TO BK-DUE-DATE-RAW
           END-IF
           IF BK-OVERRIDE-REASON NOT = SPACES
              AND BK-OVERRIDE-REASON NOT = LOW-VALUES
               MOVE 4 TO CA-RETURN-CODE
               MOVE "APK0012" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Due date overridden with a recorded reason; "
                      "the contractual date is retained"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           ELSE
               IF CA-RETURN-CODE < 8
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Due date derived from the recorded terms"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               END-IF
           END-IF
           IF BK-DATE-RECEIVED > BK-DOCUMENT-DATE
               MOVE SPACES TO BK-NOTE
               STRING "Internal receipt was later than the document "
                      "date; the contractual due date is unchanged"
                   DELIMITED BY SIZE INTO BK-NOTE
           END-IF.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE CONTROLLER'S EXPLICIT EXCEPTION: RECOGNISE A GENUINE
      *> LIABILITY THAT CANNOT YET BE MATCHED, IN A DESIGNATED PENDING
      *> MATCH CLEARING ACCOUNT, WITH PAYMENT STILL BLOCKED.  IT IS NOT
      *> A SILENT NORMAL PATH FALLBACK - IT NEEDS AUTHORITY.
      *>----------------------------------------------------------------
       5000-PENDING SECTION.
       5000-START.
           INITIALIZE WS-APSVC
           MOVE "PERM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "AP.INVOICE.APPROVE" TO SVC-TEXT-1
           CALL "APSVC" USING WS-APSVC
           IF SVC-FLAG-1 NOT = "Y"
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APK0007" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Recognising an unmatched invoice in pending "
                      "match clearing needs controller approval"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 5000-EXIT
           END-IF
           IF BK-PENDING-GBP = 0
               MOVE "APK0008" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The value to recognise in pending match "
                      "clearing is required"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           IF BK-CLEARING-ACCOUNT = SPACES
               MOVE "2015" TO BK-CLEARING-ACCOUNT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.PurchaseInvoice SET "
                  "PendingMatchClearingGBP = ?, MatchState = "
                  "'UNMATCHED', AccountingState = 'AWAITAPPROVAL', "
                  "PaymentEligibility = 'BLOCKED', BlockReason = ?, "
                  "ApprovedBy = ?, ApprovedAt = SYSDATETIME(), "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE CompanyCode = ? AND PurchaseInvoiceId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE BK-PENDING-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE SPACES TO SQLIO-PARM-VAL(2)
           STRING "Recognised in pending match clearing; payment "
                  "remains blocked until matching completes"
               DELIMITED BY SIZE INTO SQLIO-PARM-VAL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE CA-USER TO SQLIO-PARM-VAL(4)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(5)
           MOVE BK-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE 6 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
           MOVE "AWAITAPPROVAL" TO BK-ACCT-STATE
           MOVE "BLOCKED" TO BK-PAY-ELIGIBILITY
           MOVE "Y" TO BK-IS-BLOCKED
           MOVE BK-PENDING-GBP TO WS-MONEY-ED
           MOVE 4 TO CA-RETURN-CODE
           MOVE "APK0013" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "GBP " FUNCTION TRIM(WS-MONEY-ED)
                  " recognised in pending match clearing; the "
                  "invoice stays payment blocked"
               DELIMITED BY SIZE INTO CA-MESSAGE
           ADD 1 TO CA-MSG-COUNT
           PERFORM 2400-AUDIT THRU 2400-EXIT.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       7000-READ-INVOICE SECTION.
       7000-START.
           MOVE SPACES TO WS-INV-ROW
           IF BK-INVOICE-ID = 0 AND BK-REGISTER-NUMBER = SPACES
               GO TO 7000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CAST(PurchaseInvoiceId AS "
                  "varchar(24)), SupplierCode, ISNULL(TermsCode,''), "
                  "CONVERT(char(8), DocumentDate, 112), "
                  "CONVERT(char(8), DateReceived, 112), "
                  "CAST(GrossAmountGBP AS varchar(24)), "
                  "ISNULL(OwnerUser,''), AccountingState, "
                  "PaymentEligibility FROM ap.PurchaseInvoice "
                  "WHERE CompanyCode = ? AND ((? <> 0 AND "
                  "PurchaseInvoiceId = ?) OR (? <> '' AND "
                  "RegisterNumber = ?))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE BK-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE BK-REGISTER-NUMBER TO SQLIO-PARM-VAL(4)
           MOVE BK-REGISTER-NUMBER TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 9 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 12 TO SQLIO-COL-WIDTH(3)
           MOVE 8 TO SQLIO-COL-WIDTH(4)
           MOVE 8 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 20 TO SQLIO-COL-WIDTH(7)
           MOVE 14 TO SQLIO-COL-WIDTH(8)
           MOVE 14 TO SQLIO-COL-WIDTH(9)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-INV-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 7000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 7000-EXIT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R-ID)
           MOVE WS-NUM TO BK-INVOICE-ID
           MOVE WS-R-SUPPLIER TO BK-SUPPLIER
           IF BK-TERMS-CODE = SPACES
               MOVE WS-R-TERMS TO BK-TERMS-CODE
           END-IF
           IF BK-DOCUMENT-DATE = 0
               COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R-DOCDATE)
               MOVE WS-NUM TO BK-DOCUMENT-DATE
           END-IF
           IF BK-DATE-RECEIVED = 0
               COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R-RECEIVED)
               MOVE WS-NUM TO BK-DATE-RECEIVED
           END-IF
           IF BK-AMOUNT-GBP = 0
               COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R-GROSS)
               MOVE WS-NUM TO BK-AMOUNT-GBP
           END-IF
           MOVE WS-R-ACCTSTATE TO BK-ACCT-STATE.
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
           MOVE "APK0099" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "APBLOCK does not support function "
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
               MOVE "SQL failure in APBLOCK" TO CA-MESSAGE
           END-IF
           ADD 1 TO CA-MSG-COUNT
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       9300-EXIT.
           EXIT.
