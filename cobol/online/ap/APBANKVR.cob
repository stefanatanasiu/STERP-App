      *>================================================================
      *> PROGRAM-ID : APBANKVR
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : CONTROL OF A NEW OR CHANGED PAYEE BANK
      *>              DESTINATION.  THREE SEPARATE PIECES OF EVIDENCE
      *>              ARE REQUIRED BEFORE THE DESTINATION MAY BE PAID:
      *>                1 CONFIRMATION OF PAYEE, WITH AN AUTHORISED
      *>                  DECISION WHEN IT IS NOT A FULL MATCH
      *>                2 AN INDEPENDENT CALL-BACK TO A CONTACT THAT IS
      *>                  ALREADY HELD ON RECORD - NEVER TO A NUMBER
      *>                  SUPPLIED WITH THE CHANGE REQUEST
      *>                3 APPROVAL BY SOMEONE OTHER THAN THE PREPARER
      *>              APPROVING A CHANGE INVALIDATES ANY PAYMENT
      *>              APPROVAL THAT STILL RELIES ON THE OLD DETAILS;
      *>              SUBMITTED INSTRUCTIONS KEEP THEIR OWN SNAPSHOT.
      *> CALLED BY  : .NET SUPPLIER BANK FORM, APPAYPRP, APBACS
      *> INTERFACE  : CALL "APBANKVR" USING WS-APBANKVR-CA
      *>              COPYBOOK APBANKVRCA
      *> CALLS      : SQLIO, APSVC
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-07-11 DJM  ORIGINAL - BACS BENEFICIARY AMENDMENT CONTROL
      *> 1997-03-05 RJH  CR0714 PREPARER MAY NOT BE THE APPROVER
      *> 2017-06-19 AJS  CR3390 IBAN AND BIC FOR SEPA PAYMENTS
      *> 2026-10-05 STERP CONFIRMATION OF PAYEE AND CALL-BACK EVIDENCE
      *>                 (AP-027), APPROVAL INVALIDATION (AP-002)
      *>================================================================
      *> REQ: AP-002, AP-027, COM-003, COM-008, D-07, D-16
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APBANKVR.
       AUTHOR. D J MILLER.
       DATE-WRITTEN. 11/07/1994.
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
               88  WS-BANK-FOUND                 VALUE "Y".
      *>   7000-READ-BANK REFRESHES THE COMMAREA FROM THE STORED ROW,
      *>   SO THE CALLER'S OWN COP AND CALL-BACK EVIDENCE IS HELD HERE
      *>   OVER THE READ AND PUT BACK BEFORE IT IS EDITED (AP-027).
       01  WS-SUPPLIED.
           05  WS-IN-COP-RESULT        PIC X(12).
           05  WS-IN-COP-DECISION      PIC X(200).
           05  WS-IN-CALLBACK-SRC      PIC X(120).
       01  WS-BANK-ROW.
           05  WS-K-ID                 PIC X(12).
           05  WS-K-BENEFICIARY        PIC X(120).
           05  WS-K-COUNTRY            PIC X(2).
           05  WS-K-CURRENCY           PIC X(3).
           05  WS-K-SORT               PIC X(6).
           05  WS-K-ACCOUNT            PIC X(8).
           05  WS-K-IBAN               PIC X(34).
           05  WS-K-BIC                PIC X(11).
           05  WS-K-BANKNAME           PIC X(60).
           05  WS-K-METHOD             PIC X(10).
           05  WS-K-COP                PIC X(12).
           05  WS-K-COP-DECISION       PIC X(200).
           05  WS-K-CALLBACK-AT        PIC X(10).
           05  WS-K-CALLBACK-BY        PIC X(20).
           05  WS-K-CALLBACK-SRC       PIC X(120).
           05  WS-K-STATUS             PIC X(12).
           05  WS-K-PREPARED           PIC X(20).
           05  WS-K-APPROVED           PIC X(20).
           05  WS-K-SUPERSEDES         PIC X(12).
           05  WS-K-REQSOURCE          PIC X(30).
       01  WS-ONE-COL.
           05  WS-C-VALUE              PIC X(64).
       01  WS-TWO-COL.
           05  WS-C2-ONE               PIC X(64).
           05  WS-C2-TWO               PIC X(64).
       01  WS-WORK.
           05  WS-NUM                  PIC S9(15)V9(6) VALUE 0.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-DIGITS               PIC 9(4) VALUE 0.
           05  WS-LEN                  PIC S9(4) VALUE 0.
           05  WS-CH                   PIC X VALUE SPACE.
           05  WS-COUNT-ED             PIC Z(5)9.
           05  WS-HASH-TEXT            PIC X(300) VALUE SPACES.
       LINKAGE SECTION.
       COPY APBANKVRCA.
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
               WHEN "PREPARE"   PERFORM 2000-PREPARE THRU 2000-EXIT
               WHEN "COP"       PERFORM 3000-COP THRU 3000-EXIT
               WHEN "CALLBACK"  PERFORM 4000-CALLBACK THRU 4000-EXIT
               WHEN "APPROVE"   PERFORM 5000-APPROVE THRU 5000-EXIT
               WHEN "CHECK"     PERFORM 6000-CHECK THRU 6000-EXIT
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
           MOVE "APBANKVR" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "AP-027" TO CA-REQ-REF
           MOVE SPACES TO BV-NOTE
           MOVE "N" TO BV-USABLE
           MOVE "N" TO BV-COP-DONE
           MOVE "N" TO BV-CALLBACK-DONE
           MOVE 0 TO BV-INVALIDATED-CNT
           MOVE 0 TO BV-UNSUBMITTED-CNT
           MOVE FUNCTION UPPER-CASE(BV-SUPPLIER) TO BV-SUPPLIER
           MOVE FUNCTION UPPER-CASE(BV-BANK-REF) TO BV-BANK-REF
           MOVE FUNCTION UPPER-CASE(CA-COMPANY) TO CA-COMPANY
           IF BV-BANK-REF = SPACES OR BV-BANK-REF = LOW-VALUES
               MOVE "MAIN" TO BV-BANK-REF
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1500-VALIDATE SECTION.
       1500-START.
           IF CA-COMPANY = SPACES OR CA-COMPANY = LOW-VALUES
               MOVE "APB0001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1500-EXIT
           END-IF
           IF BV-SUPPLIER = SPACES OR BV-SUPPLIER = LOW-VALUES
               MOVE "APB0002" TO CA-REASON-CODE
               MOVE "A supplier account code is required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PREPARE RECORDS THE REQUESTED DESTINATION.  IT IS DELIBERATELY
      *> NOT USABLE: THE ROW IS WRITTEN AS PENDING AND THE CALLER IS
      *> TOLD WHAT EVIDENCE IS STILL OUTSTANDING (04, NOT 00).
      *>----------------------------------------------------------------
       2000-PREPARE SECTION.
       2000-START.
           MOVE "AP.BANK.MAINT" TO SVC-TEXT-1
           PERFORM 8000-CHECK-PERM THRU 8000-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-EDIT-DETAILS THRU 2100-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-FIND-CURRENT THRU 2200-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           PERFORM 2300-INSERT-ROW THRU 2300-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           PERFORM 2400-AUDIT-PREPARE THRU 2400-EXIT
           MOVE "PENDING" TO BV-STATUS
           MOVE "N" TO BV-USABLE
           MOVE 4 TO CA-RETURN-CODE
           MOVE "APB0010" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Destination recorded as pending. Confirmation of "
                  "Payee, an independent call-back and approval are "
                  "required before it may be paid"
               DELIMITED BY SIZE INTO CA-MESSAGE
           ADD 1 TO CA-MSG-COUNT.
       2000-EXIT.
           EXIT.

       2100-EDIT-DETAILS SECTION.
       2100-START.
           IF BV-BENEFICIARY = SPACES OR BV-BENEFICIARY = LOW-VALUES
               MOVE "APB0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The beneficiary name exactly as held by the "
                      "bank is required" DELIMITED BY SIZE
                   INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2100-EXIT
           END-IF
           IF BV-COUNTRY = SPACES OR BV-COUNTRY = LOW-VALUES
               MOVE "GB" TO BV-COUNTRY
           END-IF
           IF BV-CURRENCY = SPACES OR BV-CURRENCY = LOW-VALUES
               MOVE "GBP" TO BV-CURRENCY
           END-IF
           IF BV-COUNTRY = "GB"
               PERFORM 2110-EDIT-UK THRU 2110-EXIT
           ELSE
               PERFORM 2120-EDIT-INTL THRU 2120-EXIT
           END-IF.
       2100-EXIT.
           EXIT.

       2110-EDIT-UK SECTION.
       2110-START.
           MOVE BV-SORT-CODE TO WS-HASH-TEXT
           MOVE 6 TO WS-LEN
           PERFORM 2130-COUNT-DIGITS THRU 2130-EXIT
           IF WS-DIGITS NOT = 6
               MOVE "APB0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A United Kingdom sort code must be six digits"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2110-EXIT
           END-IF
           MOVE BV-ACCOUNT-NO TO WS-HASH-TEXT
           MOVE 8 TO WS-LEN
           PERFORM 2130-COUNT-DIGITS THRU 2130-EXIT
           IF WS-DIGITS NOT = 8
               MOVE "APB0005" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A United Kingdom account number must be "
                      "eight digits" DELIMITED BY SIZE
                   INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       2110-EXIT.
           EXIT.

       2120-EDIT-INTL SECTION.
       2120-START.
           IF BV-IBAN = SPACES OR BV-IBAN = LOW-VALUES
               MOVE "APB0006" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "An IBAN is required for an international "
                      "payment destination" DELIMITED BY SIZE
                   INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2120-EXIT
           END-IF
           IF BV-BIC = SPACES OR BV-BIC = LOW-VALUES
               MOVE "APB0007" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A BIC is required for an international "
                      "payment destination" DELIMITED BY SIZE
                   INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       2120-EXIT.
           EXIT.

       2130-COUNT-DIGITS SECTION.
       2130-START.
           MOVE 0 TO WS-DIGITS
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LEN
               MOVE WS-HASH-TEXT(WS-I:1) TO WS-CH
               IF WS-CH >= "0" AND WS-CH <= "9"
                   ADD 1 TO WS-DIGITS
               END-IF
           END-PERFORM.
       2130-EXIT.
           EXIT.

       2200-FIND-CURRENT SECTION.
       2200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CAST(SupplierBankId AS varchar(12))"
                  " FROM ap.SupplierBank WHERE CompanyCode = ? "
                  "AND SupplierCode = ? AND Status = 'APPROVED' "
                  "AND (ValidTo IS NULL OR ValidTo >= CAST("
                  "SYSDATETIME() AS date)) ORDER BY SupplierBankId DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE BV-SUPPLIER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2200-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 0 TO BV-SUPERSEDES-ID
           ELSE
               COMPUTE WS-NUM = FUNCTION NUMVAL(WS-C-VALUE)
               MOVE WS-NUM TO BV-SUPERSEDES-ID
           END-IF.
       2200-EXIT.
           EXIT.

       2300-INSERT-ROW SECTION.
       2300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ap.SupplierBank (CompanyCode, SupplierCode,"
                  " BankRef, BeneficiaryName, CountryCode, "
                  "CurrencyCode, SortCode, AccountNumber, RollNumber,"
                  " Iban, Bic, BankName, PaymentMethod, ValidFrom, "
                  "Status, PreparedBy, SupersedesBankId, "
                  "ChangeRequestSource, IsSensitive, CreatedBy) "
                  "SELECT ?, ?, ?, ?, ?, ?, NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(?,''), NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(?,''), ?, "
                  "CAST(? AS date), 'PENDING', ?, NULLIF(?,0), ?, 1, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 17 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE BV-SUPPLIER TO SQLIO-PARM-VAL(2)
           MOVE BV-BANK-REF TO SQLIO-PARM-VAL(3)
           MOVE BV-BENEFICIARY TO SQLIO-PARM-VAL(4)
           MOVE BV-COUNTRY TO SQLIO-PARM-VAL(5)
           MOVE BV-CURRENCY TO SQLIO-PARM-VAL(6)
           MOVE BV-SORT-CODE TO SQLIO-PARM-VAL(7)
           MOVE BV-ACCOUNT-NO TO SQLIO-PARM-VAL(8)
           MOVE BV-ROLL-NUMBER TO SQLIO-PARM-VAL(9)
           MOVE BV-IBAN TO SQLIO-PARM-VAL(10)
           MOVE BV-BIC TO SQLIO-PARM-VAL(11)
           MOVE BV-BANK-NAME TO SQLIO-PARM-VAL(12)
           IF BV-PAY-METHOD = SPACES
               MOVE "BACS" TO SQLIO-PARM-VAL(13)
           ELSE
               MOVE BV-PAY-METHOD TO SQLIO-PARM-VAL(13)
           END-IF
           IF BV-VALID-FROM = 0
               MOVE CA-BUSINESS-DATE TO BV-VALID-FROM
           END-IF
           MOVE BV-VALID-FROM TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(14)
           MOVE CA-USER TO SQLIO-PARM-VAL(15)
           MOVE BV-SUPERSEDES-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(16)
           MOVE BV-REQUEST-SOURCE TO SQLIO-PARM-VAL(17)
           MOVE CA-USER TO SQLIO-PARM-VAL(18)
           MOVE 18 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 18
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2300-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           PERFORM 7000-READ-BANK THRU 7000-EXIT.
       2300-EXIT.
           EXIT.

       2400-AUDIT-PREPARE SECTION.
       2400-START.
           INITIALIZE WS-APSVC
           MOVE "AUDIT" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "ap.SupplierBank" TO SVC-AUD-ENTITY
           MOVE SPACES TO SVC-AUD-KEY
           STRING FUNCTION TRIM(BV-SUPPLIER) "/"
                  FUNCTION TRIM(BV-BANK-REF)
               DELIMITED BY SIZE INTO SVC-AUD-KEY
           MOVE "PREPARE" TO SVC-AUD-ACTION
           MOVE "BankDetails" TO SVC-AUD-FIELD
           MOVE BV-REQUEST-SOURCE TO SVC-AUD-REASON
           MOVE "Y" TO SVC-AUD-SENS
           CALL "APSVC" USING WS-APSVC.
       2400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AP-027: A CLOSE MATCH OR A NO MATCH NEEDS A RECORDED DECISION
      *> BY AN AUTHORISED PERSON.  WITHOUT ONE THE REQUEST IS HELD (06)
      *> RATHER THAN QUIETLY ACCEPTED.
      *>----------------------------------------------------------------
       3000-COP SECTION.
       3000-START.
           MOVE "AP.BANK.VERIFY" TO SVC-TEXT-1
           PERFORM 8000-CHECK-PERM THRU 8000-EXIT
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
           MOVE BV-COP-RESULT TO WS-IN-COP-RESULT
           MOVE BV-COP-DECISION TO WS-IN-COP-DECISION
           PERFORM 7000-READ-BANK THRU 7000-EXIT
           MOVE WS-IN-COP-RESULT TO BV-COP-RESULT
           MOVE WS-IN-COP-DECISION TO BV-COP-DECISION
           IF WS-FATAL OR NOT WS-BANK-FOUND
               GO TO 3000-EXIT
           END-IF
           IF NOT (BV-COP-MATCH OR BV-COP-CLOSE OR BV-COP-NOMATCH
                   OR BV-COP-UNAVAIL)
               MOVE "APB0011" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The Confirmation of Payee result must be "
                      "MATCH, CLOSEMATCH, NOMATCH or UNAVAILABLE"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           IF NOT BV-COP-MATCH
             AND (BV-COP-DECISION = SPACES
                  OR BV-COP-DECISION = LOW-VALUES)
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APB0012" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A " FUNCTION TRIM(BV-COP-RESULT)
                      " Confirmation of Payee result needs an "
                      "authorised decision before the destination "
                      "may be used" DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.SupplierBank SET CopResult = ?, "
                  "CopCheckedAt = SYSDATETIME(), CopReference = ?, "
                  "CopDecision = NULLIF(?,''), ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE SupplierBankId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE BV-COP-RESULT TO SQLIO-PARM-VAL(1)
           MOVE BV-COP-REFERENCE TO SQLIO-PARM-VAL(2)
           MOVE BV-COP-DECISION TO SQLIO-PARM-VAL(3)
           MOVE CA-USER TO SQLIO-PARM-VAL(4)
           MOVE BV-BANK-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
           MOVE "Y" TO BV-COP-DONE
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Confirmation of Payee result "
                  FUNCTION TRIM(BV-COP-RESULT) " recorded"
               DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE WS-APSVC
           MOVE "AUDIT" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "ap.SupplierBank" TO SVC-AUD-ENTITY
           MOVE BV-BANK-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SVC-AUD-KEY
           MOVE "COPCHECK" TO SVC-AUD-ACTION
           MOVE "CopResult" TO SVC-AUD-FIELD
           MOVE BV-COP-RESULT TO SVC-AUD-NEW
           MOVE BV-COP-DECISION TO SVC-AUD-REASON
           MOVE BV-COP-REFERENCE TO SVC-AUD-AUTH
           MOVE "Y" TO SVC-AUD-SENS
           CALL "APSVC" USING WS-APSVC.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AP-027: THE CALL-BACK MUST BE TO A CONTACT ALREADY HELD ON
      *> RECORD.  A NUMBER SUPPLIED WITH THE CHANGE REQUEST - TYPICALLY
      *> IN THE SAME EMAIL - IS EXACTLY THE FRAUD THIS CONTROL EXISTS
      *> TO STOP, SO IT IS REFUSED (08), NOT WARNED ABOUT.
      *>----------------------------------------------------------------
       4000-CALLBACK SECTION.
       4000-START.
           MOVE "AP.BANK.VERIFY" TO SVC-TEXT-1
           PERFORM 8000-CHECK-PERM THRU 8000-EXIT
           IF WS-FATAL
               GO TO 4000-EXIT
           END-IF
           MOVE BV-CALLBACK-SOURCE TO WS-IN-CALLBACK-SRC
           PERFORM 7000-READ-BANK THRU 7000-EXIT
           MOVE WS-IN-CALLBACK-SRC TO BV-CALLBACK-SOURCE
           IF WS-FATAL OR NOT WS-BANK-FOUND
               GO TO 4000-EXIT
           END-IF
           IF BV-CALLBACK-SOURCE = SPACES
              OR BV-CALLBACK-SOURCE = LOW-VALUES
               MOVE "APB0013" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The call-back must say which contact already "
                      "on record was telephoned"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           IF BV-CALLBACK-ON-FILE NOT = "Y"
               MOVE "APB0014" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The call-back was not made to a contact "
                      "already held on record, so it cannot verify "
                      "this change" DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           IF WS-K-COP = SPACES OR WS-K-COP = LOW-VALUES
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APB0015" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Record the Confirmation of Payee result "
                      "before the call-back verification"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 4000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.SupplierBank SET CallBackVerifiedAt = "
                  "SYSDATETIME(), CallBackVerifiedBy = ?, "
                  "CallBackContactSource = ?, CallBackNote = "
                  "NULLIF(?,''), Status = 'VERIFIED', ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE SupplierBankId = ?"
                  " AND Status = 'PENDING'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE BV-CALLBACK-SOURCE TO SQLIO-PARM-VAL(2)
           MOVE BV-CALLBACK-NOTE TO SQLIO-PARM-VAL(3)
           MOVE CA-USER TO SQLIO-PARM-VAL(4)
           MOVE BV-BANK-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-FATAL
               GO TO 4000-EXIT
           END-IF
           MOVE "Y" TO BV-CALLBACK-DONE
           MOVE "VERIFIED" TO BV-STATUS
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Independent call-back recorded; the change now "
                  "needs approval by someone other than the preparer"
               DELIMITED BY SIZE INTO CA-MESSAGE
           INITIALIZE WS-APSVC
           MOVE "AUDIT" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "ap.SupplierBank" TO SVC-AUD-ENTITY
           MOVE BV-BANK-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SVC-AUD-KEY
           MOVE "CALLBACK" TO SVC-AUD-ACTION
           MOVE "CallBackVerifiedBy" TO SVC-AUD-FIELD
           MOVE CA-USER TO SVC-AUD-NEW
           MOVE BV-CALLBACK-SOURCE TO SVC-AUD-REASON
           MOVE "Y" TO SVC-AUD-SENS
           CALL "APSVC" USING WS-APSVC.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> APPROVAL.  core.usp_RequestApproval AND core.usp_DecideApproval
      *> ENFORCE THE D-07 MATRIX, SELF APPROVAL AND THE SoD RULE
      *> SOD-BANK, SO THE PREPARER CANNOT APPROVE THE CHANGE.
      *>----------------------------------------------------------------
       5000-APPROVE SECTION.
       5000-START.
           MOVE "AP.BANK.APPROVE" TO SVC-TEXT-1
           PERFORM 8000-CHECK-PERM THRU 8000-EXIT
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
           PERFORM 7000-READ-BANK THRU 7000-EXIT
           IF WS-FATAL OR NOT WS-BANK-FOUND
               GO TO 5000-EXIT
           END-IF
           IF WS-K-STATUS NOT = "VERIFIED"
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APB0016" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The destination has not been independently "
                      "verified, so it cannot be approved"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 5000-EXIT
           END-IF
           IF FUNCTION TRIM(WS-K-PREPARED) = FUNCTION TRIM(CA-USER)
               MOVE "APB0017" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The preparer of a bank detail change cannot "
                      "approve it; an independent approver is "
                      "required" DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           PERFORM 5100-REQUEST-APPROVAL THRU 5100-EXIT
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
           PERFORM 5200-DECIDE THRU 5200-EXIT
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
           IF BV-DECISION = "REJECTED"
               PERFORM 5300-MARK-REJECTED THRU 5300-EXIT
               GO TO 5000-EXIT
           END-IF
           PERFORM 5400-MARK-APPROVED THRU 5400-EXIT
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
           PERFORM 5500-INVALIDATE THRU 5500-EXIT
           MOVE "APPROVED" TO BV-STATUS
           MOVE "Y" TO BV-USABLE
           MOVE BV-INVALIDATED-CNT TO WS-COUNT-ED
           MOVE SPACES TO CA-MESSAGE
           IF BV-INVALIDATED-CNT > 0
               MOVE 4 TO CA-RETURN-CODE
               MOVE "APB0018" TO CA-REASON-CODE
               STRING "Destination approved. "
                      FUNCTION TRIM(WS-COUNT-ED)
                      " payment approval(s) relying on the old "
                      "details were invalidated and must be "
                      "re-approved" DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           ELSE
               MOVE 0 TO CA-RETURN-CODE
               STRING "Destination approved and available for payment"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       5000-EXIT.
           EXIT.

       5100-REQUEST-APPROVAL SECTION.
       5100-START.
           PERFORM 7100-BUILD-HASH THRU 7100-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC core.usp_RequestApproval ?, 'SUPPLIERBANK', "
                  "?, 'PAYEEBANK', ?, ?, NULL, 'GBP', NULL, NULL, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SPACES TO SQLIO-PARM-VAL(2)
           MOVE BV-BANK-ID TO SQLIO-INT-EDIT
           STRING FUNCTION TRIM(BV-SUPPLIER) "/"
                  FUNCTION TRIM(SQLIO-INT-EDIT)
               DELIMITED BY SIZE INTO SQLIO-PARM-VAL(2)
           MOVE BV-CONTENT-HASH TO SQLIO-PARM-VAL(3)
           MOVE WS-K-PREPARED TO SQLIO-PARM-VAL(4)
           MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           IF CA-CORRELATION-ID = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(5)
           END-IF
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 64 TO SQLIO-COL-WIDTH(1)
           MOVE 64 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO-COL
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 5100-EXIT
           END-IF
           IF NOT SQLIO-NOTFOUND
               COMPUTE WS-NUM = FUNCTION NUMVAL(WS-C2-ONE)
               MOVE WS-NUM TO BV-APPROVAL-ID
           END-IF.
       5100-EXIT.
           EXIT.

       5200-DECIDE SECTION.
       5200-START.
           IF BV-DECISION = SPACES OR BV-DECISION = LOW-VALUES
               MOVE "APPROVED" TO BV-DECISION
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC core.usp_DecideApproval ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE BV-APPROVAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           MOVE BV-DECISION TO SQLIO-PARM-VAL(3)
           MOVE BV-COP-DECISION TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               MOVE "Y" TO WS-FATAL-SW
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APB0019" TO CA-REASON-CODE
               MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       5200-EXIT.
           EXIT.

       5300-MARK-REJECTED SECTION.
       5300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.SupplierBank SET Status = 'REJECTED', "
                  "ApprovalId = NULLIF(?,0), ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE SupplierBankId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE BV-APPROVAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           MOVE BV-BANK-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           MOVE "REJECTED" TO BV-STATUS
           MOVE "N" TO BV-USABLE
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "The bank detail change was rejected; the "
                  "previously approved destination remains in force"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       5300-EXIT.
           EXIT.

       5400-MARK-APPROVED SECTION.
       5400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.SupplierBank SET Status = 'APPROVED', "
                  "ApprovedBy = ?, ApprovedAt = SYSDATETIME(), "
                  "ApprovalId = NULLIF(?,0), ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE SupplierBankId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE BV-APPROVAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE BV-BANK-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-FATAL
               GO TO 5400-EXIT
           END-IF
           IF BV-SUPERSEDES-ID > 0
               MOVE SPACES TO SQLIO-STMT
               STRING "UPDATE ap.SupplierBank SET Status = "
                      "'SUPERSEDED', ValidTo = CAST(? AS date), "
                      "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                      "WHERE SupplierBankId = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 3 TO SQLIO-PARM-CNT
               MOVE CA-BUSINESS-DATE TO SQLIO-INT-EDIT
               MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
               MOVE CA-USER TO SQLIO-PARM-VAL(2)
               MOVE BV-SUPERSEDES-ID TO SQLIO-INT-EDIT
               MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
                   MOVE "N" TO SQLIO-PARM-NULL(WS-I)
               END-PERFORM
               PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           END-IF
           INITIALIZE WS-APSVC
           MOVE "AUDIT" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "ap.SupplierBank" TO SVC-AUD-ENTITY
           MOVE BV-BANK-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SVC-AUD-KEY
           MOVE "APPROVE" TO SVC-AUD-ACTION
           MOVE "Status" TO SVC-AUD-FIELD
           MOVE "VERIFIED" TO SVC-AUD-OLD
           MOVE "APPROVED" TO SVC-AUD-NEW
           MOVE WS-K-COP TO SVC-AUD-REASON
           MOVE BV-CONTENT-HASH TO SVC-AUD-AUTH
           MOVE "Y" TO SVC-AUD-SENS
           CALL "APSVC" USING WS-APSVC.
       5400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AP-002: AN APPROVED DESTINATION CHANGE INVALIDATES EVERY
      *> PAYMENT APPROVAL THAT WAS GIVEN AGAINST THE OLD BENEFICIARY.
      *> ALREADY SUBMITTED INSTRUCTIONS ARE LEFT ALONE - THEY KEEP
      *> THEIR OWN APPROVED SNAPSHOT AND ARE CORRECTED, IF AT ALL, BY
      *> AN AUTHORISED CANCELLATION AND REPLACEMENT.
      *>----------------------------------------------------------------
       5500-INVALIDATE SECTION.
       5500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC ap.usp_InvalidatePaymentApprovals ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE BV-SUPPLIER TO SQLIO-PARM-VAL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE SPACES TO SQLIO-PARM-VAL(4)
           STRING "Payee bank details changed and approved"
               DELIMITED BY SIZE INTO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 64 TO SQLIO-COL-WIDTH(1)
           MOVE 64 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO-COL
           IF SQLIO-ERROR
               MOVE 0 TO BV-INVALIDATED-CNT
               MOVE "CLOS" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-TWO-COL
               GO TO 5500-EXIT
           END-IF
           IF NOT SQLIO-NOTFOUND
               COMPUTE WS-NUM = FUNCTION NUMVAL(WS-C2-ONE)
               MOVE WS-NUM TO BV-INVALIDATED-CNT
               COMPUTE WS-NUM = FUNCTION NUMVAL(WS-C2-TWO)
               MOVE WS-NUM TO BV-UNSUBMITTED-CNT
           END-IF.
       5500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> CHECK IS THE GATE THE PAYMENT RUN USES.  BANKING DETAIL IS
      *> RESTRICTED EVEN WHERE THE PAYABLE AMOUNT IS VISIBLE (D-17), SO
      *> THE ACCOUNT NUMBER IS ONLY RETURNED TO A CALLER WHO HOLDS
      *> AP.BANK.VIEW.
      *>----------------------------------------------------------------
       6000-CHECK SECTION.
       6000-START.
           PERFORM 7000-READ-BANK THRU 7000-EXIT
           IF WS-FATAL
               GO TO 6000-EXIT
           END-IF
           IF NOT WS-BANK-FOUND
               MOVE 16 TO CA-RETURN-CODE
               MOVE "APB0020" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No payment destination is held for supplier "
                      FUNCTION TRIM(BV-SUPPLIER)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 6000-EXIT
           END-IF
           MOVE "AP.BANK.VIEW" TO SVC-TEXT-1
           INITIALIZE WS-APSVC
           MOVE "PERM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "AP.BANK.VIEW" TO SVC-TEXT-1
           CALL "APSVC" USING WS-APSVC
           IF SVC-FLAG-1 NOT = "Y"
               MOVE SPACES TO BV-SORT-CODE BV-ACCOUNT-NO
               MOVE SPACES TO BV-IBAN BV-ROLL-NUMBER
           END-IF
           IF WS-K-STATUS = "APPROVED"
               MOVE "Y" TO BV-USABLE
               MOVE 0 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Approved destination "
                      FUNCTION TRIM(BV-BANK-REF) " is available"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               MOVE "N" TO BV-USABLE
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APB0021" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The payment destination is "
                      FUNCTION TRIM(WS-K-STATUS)
                      " and has not completed verification and "
                      "approval, so it cannot be paid"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       6000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       7000-READ-BANK SECTION.
       7000-START.
           MOVE "N" TO WS-FOUND-SW
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CAST(SupplierBankId AS varchar(12))"
                  ", BeneficiaryName, CountryCode, CurrencyCode, "
                  "ISNULL(SortCode,''), ISNULL(AccountNumber,''), "
                  "ISNULL(Iban,''), ISNULL(Bic,''), "
                  "ISNULL(BankName,''), PaymentMethod, "
                  "ISNULL(CopResult,''), ISNULL(CopDecision,''), "
                  "ISNULL(CONVERT(char(10), CallBackVerifiedAt, 126)"
                  ",''), ISNULL(CallBackVerifiedBy,''), "
                  "ISNULL(CallBackContactSource,''), Status, "
                  "ISNULL(PreparedBy,''), ISNULL(ApprovedBy,''), "
                  "CAST(ISNULL(SupersedesBankId,0) AS varchar(12)), "
                  "ISNULL(ChangeRequestSource,'') "
                  "FROM ap.SupplierBank WHERE CompanyCode = ? "
                  "AND SupplierCode = ? AND (? = 0 OR "
                  "SupplierBankId = ?) AND (? = '' OR BankRef = ?) "
                  "ORDER BY CASE Status WHEN 'PENDING' THEN 1 WHEN "
                  "'VERIFIED' THEN 2 WHEN 'APPROVED' THEN 3 ELSE 4 "
                  "END, SupplierBankId DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE BV-SUPPLIER TO SQLIO-PARM-VAL(2)
           MOVE BV-BANK-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           IF BV-BANK-ID > 0
               MOVE SPACES TO SQLIO-PARM-VAL(5)
               MOVE SPACES TO SQLIO-PARM-VAL(6)
           ELSE
               MOVE BV-BANK-REF TO SQLIO-PARM-VAL(5)
               MOVE BV-BANK-REF TO SQLIO-PARM-VAL(6)
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 20 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 120 TO SQLIO-COL-WIDTH(2)
           MOVE 2 TO SQLIO-COL-WIDTH(3)
           MOVE 3 TO SQLIO-COL-WIDTH(4)
           MOVE 6 TO SQLIO-COL-WIDTH(5)
           MOVE 8 TO SQLIO-COL-WIDTH(6)
           MOVE 34 TO SQLIO-COL-WIDTH(7)
           MOVE 11 TO SQLIO-COL-WIDTH(8)
           MOVE 60 TO SQLIO-COL-WIDTH(9)
           MOVE 10 TO SQLIO-COL-WIDTH(10)
           MOVE 12 TO SQLIO-COL-WIDTH(11)
           MOVE 200 TO SQLIO-COL-WIDTH(12)
           MOVE 10 TO SQLIO-COL-WIDTH(13)
           MOVE 20 TO SQLIO-COL-WIDTH(14)
           MOVE 120 TO SQLIO-COL-WIDTH(15)
           MOVE 12 TO SQLIO-COL-WIDTH(16)
           MOVE 20 TO SQLIO-COL-WIDTH(17)
           MOVE 20 TO SQLIO-COL-WIDTH(18)
           MOVE 12 TO SQLIO-COL-WIDTH(19)
           MOVE 30 TO SQLIO-COL-WIDTH(20)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BANK-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 7000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 7000-EXIT
           END-IF
           MOVE "Y" TO WS-FOUND-SW
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-K-ID)
           MOVE WS-NUM TO BV-BANK-ID
           MOVE WS-K-BENEFICIARY TO BV-BENEFICIARY
           MOVE WS-K-COUNTRY TO BV-COUNTRY
           MOVE WS-K-CURRENCY TO BV-CURRENCY
           MOVE WS-K-SORT TO BV-SORT-CODE
           MOVE WS-K-ACCOUNT TO BV-ACCOUNT-NO
           MOVE WS-K-IBAN TO BV-IBAN
           MOVE WS-K-BIC TO BV-BIC
           MOVE WS-K-BANKNAME TO BV-BANK-NAME
           MOVE WS-K-METHOD TO BV-PAY-METHOD
           MOVE WS-K-COP TO BV-COP-RESULT
           MOVE WS-K-COP-DECISION TO BV-COP-DECISION
           MOVE WS-K-CALLBACK-BY TO BV-CALLBACK-BY
           MOVE WS-K-CALLBACK-SRC TO BV-CALLBACK-SOURCE
           MOVE WS-K-STATUS TO BV-STATUS
           MOVE WS-K-PREPARED TO BV-PREPARED-BY
           MOVE WS-K-APPROVED TO BV-APPROVER
           MOVE WS-K-REQSOURCE TO BV-REQUEST-SOURCE
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-K-SUPERSEDES)
           MOVE WS-NUM TO BV-SUPERSEDES-ID
           IF WS-K-COP NOT = SPACES
               MOVE "Y" TO BV-COP-DONE
           END-IF
           IF WS-K-CALLBACK-BY NOT = SPACES
               MOVE "Y" TO BV-CALLBACK-DONE
           END-IF
           IF WS-K-STATUS = "APPROVED"
               MOVE "Y" TO BV-USABLE
           END-IF.
       7000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE CONTENT HASH TIES THE APPROVAL TO THESE EXACT DETAILS.  A
      *> LATER AMENDMENT PRODUCES A DIFFERENT HASH, SO THE EARLIER
      *> APPROVAL NO LONGER APPLIES (COM-003).
      *>----------------------------------------------------------------
       7100-BUILD-HASH SECTION.
       7100-START.
           MOVE SPACES TO WS-HASH-TEXT
           STRING FUNCTION TRIM(BV-SUPPLIER) "|"
                  FUNCTION TRIM(BV-BANK-REF) "|"
                  FUNCTION TRIM(BV-BENEFICIARY) "|"
                  FUNCTION TRIM(BV-SORT-CODE) "|"
                  FUNCTION TRIM(BV-ACCOUNT-NO) "|"
                  FUNCTION TRIM(BV-IBAN) "|"
                  FUNCTION TRIM(BV-CURRENCY)
               DELIMITED BY SIZE INTO WS-HASH-TEXT
           CALL "APHASH" USING WS-HASH-TEXT BV-CONTENT-HASH.
       7100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8000-CHECK-PERM SECTION.
       8000-START.
           MOVE SVC-TEXT-1 TO WS-HASH-TEXT(201:40)
           INITIALIZE WS-APSVC
           MOVE "PERM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE WS-HASH-TEXT(201:40) TO SVC-TEXT-1
           CALL "APSVC" USING WS-APSVC
           IF SVC-FLAG-1 NOT = "Y"
               MOVE "Y" TO WS-FATAL-SW
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APB0022" TO CA-REASON-CODE
               MOVE SVC-MESSAGE TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       8000-EXIT.
           EXIT.

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
           MOVE "N" TO BV-USABLE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNC SECTION.
       9100-START.
           MOVE "APB0099" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "APBANKVR does not support function "
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
               MOVE "SQL failure in APBANKVR" TO CA-MESSAGE
           END-IF
           ADD 1 TO CA-MSG-COUNT
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       9300-EXIT.
           EXIT.
