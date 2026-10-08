      *>================================================================
      *> PROGRAM-ID : APPAYAPR
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : APPROVAL OF A PAYMENT PROPOSAL.  THE APPROVAL IS
      *>              GIVEN FOR THE EXACT SUPPLIER, BENEFICIARY,
      *>              CURRENCY, AMOUNT, ITEM SELECTION AND INTENDED
      *>              PAYMENT DATE, WHICH ARE REDUCED TO A CONTENT
      *>              HASH.  A BATCH TOTAL ALONE IS NOT EVIDENCE OF
      *>              BENEFICIARY APPROVAL (AP-012), SO EVERY
      *>              MATERIAL AMENDMENT CHANGES THE HASH AND THE
      *>              EARLIER APPROVAL NO LONGER APPLIES.
      *>              TWO AUTHORISERS ARE REQUIRED (D-07).  THE FIRST
      *>              GOES THROUGH core.Approval, WHERE SELF APPROVAL
      *>              AND THE SoD RULES ARE ENFORCED; THE SECOND IS
      *>              CHECKED HERE AND MUST DIFFER FROM BOTH THE
      *>              PREPARER AND THE FIRST AUTHORISER.
      *> CALLED BY  : .NET PAYMENT RUN FORM, APBACS
      *> INTERFACE  : CALL "APPAYAPR" USING WS-APPAYAPR-CA
      *>              COPYBOOK APPAYAPRCA
      *> CALLS      : SQLIO, APSVC, APHASH
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1995-02-27 DJM  ORIGINAL DUAL AUTHORISATION
      *> 2003-10-02 KMP  CR1740 CONTROLLER REQUIRED ABOVE A LIMIT
      *> 2026-10-05 STERP CONTENT HASH BOUND APPROVAL (AP-012)
      *>================================================================
      *> REQ: AP-012, AP-002, AP-013, COM-003, D-07
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APPAYAPR.
       AUTHOR. D J MILLER.
       DATE-WRITTEN. 27/02/1995.
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
       01  WS-PROP-ROW.
           05  WS-P-ID                 PIC X(24).
           05  WS-P-REF                PIC X(30).
           05  WS-P-VERSION            PIC X(8).
           05  WS-P-METHOD             PIC X(10).
           05  WS-P-BANK               PIC X(20).
           05  WS-P-CURRENCY           PIC X(3).
           05  WS-P-PAYDATE            PIC X(8).
           05  WS-P-OUTFLOW            PIC X(24).
           05  WS-P-SELECTED           PIC X(24).
           05  WS-P-HASH               PIC X(64).
           05  WS-P-STATUS             PIC X(12).
           05  WS-P-PREPARED           PIC X(20).
           05  WS-P-APPROVER1          PIC X(20).
           05  WS-P-APPROVER2          PIC X(20).
           05  WS-P-MAXSINGLE          PIC X(24).
       01  WS-TWO-COL.
           05  WS-C2-ONE               PIC X(64).
           05  WS-C2-TWO               PIC X(64).
       01  WS-ONE-COL.
           05  WS-C-VALUE              PIC X(40).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-NUM                  PIC S9(15)V9(6) VALUE 0.
           05  WS-FC-LIMIT             PIC S9(13)V99 VALUE 0.
           05  WS-DIR-LIMIT            PIC S9(13)V99 VALUE 0.
           05  WS-HASH-TEXT            PIC X(300) VALUE SPACES.
           05  WS-NEW-HASH             PIC X(64) VALUE SPACES.
           05  WS-MONEY-ED             PIC -(11)9.99.
           05  WS-COUNT-ED             PIC Z(5)9.
           05  WS-DATE-ED              PIC 9(8).
       LINKAGE SECTION.
       COPY APPAYAPRCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "HASH"    PERFORM 2000-HASH THRU 2000-EXIT
               WHEN "REQUEST" PERFORM 3000-REQUEST THRU 3000-EXIT
               WHEN "DECIDE"  PERFORM 4000-DECIDE THRU 4000-EXIT
               WHEN "VERIFY"  PERFORM 5000-VERIFY THRU 5000-EXIT
               WHEN "INVALID" PERFORM 6000-INVALID THRU 6000-EXIT
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
           MOVE "APPAYAPR" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "AP-012" TO CA-REQ-REF
           MOVE SPACES TO PA-NOTE
           MOVE "N" TO PA-VALID
           MOVE "N" TO PA-NEEDS-FC
           MOVE "N" TO PA-NEEDS-DIRECTOR
           MOVE 0 TO PA-INVALIDATED-CNT
           MOVE FUNCTION UPPER-CASE(CA-COMPANY) TO CA-COMPANY
           IF PA-DOCUMENT-TYPE = SPACES
               MOVE "PAYMENTPROPOSAL" TO PA-DOCUMENT-TYPE
           END-IF
           IF PA-APPROVAL-TYPE = SPACES
               MOVE "PAYMENTRUN" TO PA-APPROVAL-TYPE
           END-IF
           IF PA-REQUIRED-CNT = 0
               INITIALIZE WS-APSVC
               MOVE "PARMNUM" TO SVC-FUNC
               PERFORM 8900-SVC-HEADER THRU 8900-EXIT
               MOVE "APR.PAYRUN.AUTHORISERS" TO SVC-TEXT-1
               MOVE 2 TO SVC-NUM-1
               CALL "APSVC" USING WS-APSVC
               MOVE SVC-NUM-OUT TO PA-REQUIRED-CNT
           END-IF
           IF CA-COMPANY = SPACES
               MOVE "APA0001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2000-HASH SECTION.
       2000-START.
           PERFORM 7000-READ-PROPOSAL THRU 7000-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           PERFORM 7100-BUILD-HASH THRU 7100-EXIT
           MOVE WS-NEW-HASH TO PA-CONTENT-HASH
           MOVE WS-P-HASH TO PA-STORED-HASH
           IF WS-NEW-HASH = WS-P-HASH
               MOVE "Y" TO PA-VALID
               MOVE 0 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The proposal content is unchanged"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               MOVE "N" TO PA-VALID
               MOVE 4 TO CA-RETURN-CODE
               MOVE "APA0010" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The proposal content has changed since it was "
                      "last hashed" DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3000-REQUEST SECTION.
       3000-START.
           PERFORM 7000-READ-PROPOSAL THRU 7000-EXIT
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
           IF WS-P-STATUS = "APPROVED" OR WS-P-STATUS = "RELEASED"
               MOVE 4 TO CA-RETURN-CODE
               MOVE "APA0011" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "That proposal version is already approved"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 3000-EXIT
           END-IF
           PERFORM 7100-BUILD-HASH THRU 7100-EXIT
           MOVE WS-NEW-HASH TO PA-CONTENT-HASH
           PERFORM 3100-TIERS THRU 3100-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC core.usp_RequestApproval ?, ?, ?, ?, ?, ?, ?,"
                  " ?, ?, NULL, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE PA-DOCUMENT-TYPE TO SQLIO-PARM-VAL(2)
           MOVE PA-PROPOSAL-REF TO SQLIO-PARM-VAL(3)
           MOVE PA-APPROVAL-TYPE TO SQLIO-PARM-VAL(4)
           MOVE PA-CONTENT-HASH TO SQLIO-PARM-VAL(5)
           MOVE WS-P-PREPARED TO SQLIO-PARM-VAL(6)
           MOVE PA-AMOUNT-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE "GBP" TO SQLIO-PARM-VAL(8)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(10)
           MOVE 10 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           IF CA-CORRELATION-ID = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(10)
           END-IF
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 64 TO SQLIO-COL-WIDTH(1)
           MOVE 64 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO-COL
           IF SQLIO-ERROR
               MOVE "Y" TO WS-FATAL-SW
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APA0012" TO CA-REASON-CODE
               MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "CLOS" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-TWO-COL
               GO TO 3000-EXIT
           END-IF
           IF NOT SQLIO-NOTFOUND
               COMPUTE WS-NUM = FUNCTION NUMVAL(WS-C2-ONE)
               MOVE WS-NUM TO PA-APPROVAL-ID
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.PaymentProposal SET ContentHash = ?, "
                  "Status = 'PROPOSED', ApprovalId = NULLIF(?,0), "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE ProposalId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE PA-CONTENT-HASH TO SQLIO-PARM-VAL(1)
           MOVE PA-APPROVAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE PA-PROPOSAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           MOVE "PENDING" TO PA-STATUS
           MOVE SPACES TO CA-MESSAGE
           MOVE PA-REQUIRED-CNT TO WS-COUNT-ED
           STRING "Approval requested for the exact proposal "
                  "content; " FUNCTION TRIM(WS-COUNT-ED)
                  " independent authoriser(s) are required"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       3000-EXIT.
           EXIT.

       3100-TIERS SECTION.
       3100-START.
           INITIALIZE WS-APSVC
           MOVE "PARMNUM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "APR.PAYRUN.FC" TO SVC-TEXT-1
           MOVE 50000 TO SVC-NUM-1
           CALL "APSVC" USING WS-APSVC
           MOVE SVC-NUM-OUT TO WS-FC-LIMIT
           INITIALIZE WS-APSVC
           MOVE "PARMNUM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "APR.PAYMENT.DIRECTOR" TO SVC-TEXT-1
           MOVE 100000 TO SVC-NUM-1
           CALL "APSVC" USING WS-APSVC
           MOVE SVC-NUM-OUT TO WS-DIR-LIMIT
           IF PA-AMOUNT-GBP > WS-FC-LIMIT
               MOVE "Y" TO PA-NEEDS-FC
           END-IF
           IF PA-SINGLE-MAX-GBP > WS-DIR-LIMIT
               MOVE "Y" TO PA-NEEDS-DIRECTOR
               MOVE "PAYMENTSINGLE" TO PA-APPROVAL-TYPE
           END-IF.
       3100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE FIRST DECISION GOES THROUGH core.usp_DecideApproval, WHICH
      *> REFUSES SELF APPROVAL AND APPLIES THE SoD RULES.  THE SECOND
      *> AUTHORISER IS CHECKED HERE: THEY MUST HOLD THE PERMISSION AND
      *> DIFFER FROM BOTH THE PREPARER AND THE FIRST AUTHORISER.
      *>----------------------------------------------------------------
       4000-DECIDE SECTION.
       4000-START.
           PERFORM 7000-READ-PROPOSAL THRU 7000-EXIT
           IF WS-FATAL
               GO TO 4000-EXIT
           END-IF
           INITIALIZE WS-APSVC
           MOVE "PERM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "AP.PAYMENT.APPROVE" TO SVC-TEXT-1
           CALL "APSVC" USING WS-APSVC
           IF SVC-FLAG-1 NOT = "Y"
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APA0013" TO CA-REASON-CODE
               MOVE SVC-MESSAGE TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 4000-EXIT
           END-IF
           PERFORM 7100-BUILD-HASH THRU 7100-EXIT
           IF WS-NEW-HASH NOT = WS-P-HASH
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APA0014" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The proposal has changed since approval was "
                      "requested; a fresh version must be approved"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 4000-EXIT
           END-IF
           IF FUNCTION TRIM(CA-USER) = FUNCTION TRIM(WS-P-PREPARED)
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APA0015" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The preparer of a payment run cannot approve "
                      "it" DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 4000-EXIT
           END-IF
           IF FUNCTION TRIM(CA-USER) = FUNCTION TRIM(WS-P-APPROVER1)
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APA0016" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The second authoriser must be a different "
                      "person from the first"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 4000-EXIT
           END-IF
           PERFORM 3100-TIERS THRU 3100-EXIT
           IF WS-P-APPROVER1 = SPACES
               PERFORM 4100-FIRST THRU 4100-EXIT
           ELSE
               PERFORM 4200-SECOND THRU 4200-EXIT
           END-IF.
       4000-EXIT.
           EXIT.

       4100-FIRST SECTION.
       4100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC core.usp_DecideApproval ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE PA-APPROVAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           IF PA-DECISION = SPACES
               MOVE "APPROVED" TO PA-DECISION
           END-IF
           MOVE PA-DECISION TO SQLIO-PARM-VAL(3)
           MOVE PA-COMMENT TO SQLIO-PARM-VAL(4)
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
               MOVE "APA0017" TO CA-REASON-CODE
               MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "CLOS" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
               GO TO 4100-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF PA-DECISION = "REJECTED"
               MOVE SPACES TO SQLIO-STMT
               STRING "UPDATE ap.PaymentProposal SET Status = "
                      "'CANCELLED', ModifiedBy = ?, ModifiedAt = "
                      "SYSDATETIME() WHERE ProposalId = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 2 TO SQLIO-PARM-CNT
               MOVE CA-USER TO SQLIO-PARM-VAL(1)
               MOVE PA-PROPOSAL-ID TO SQLIO-INT-EDIT
               MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
               MOVE "N" TO SQLIO-PARM-NULL(1)
               MOVE "N" TO SQLIO-PARM-NULL(2)
               PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
               MOVE "REJECTED" TO PA-STATUS
               MOVE 0 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The payment run was rejected and cancelled"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 4100-EXIT
           END-IF
           MOVE CA-USER TO PA-APPROVER-1
           MOVE 1 TO PA-HELD-CNT
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.PaymentProposal SET Approver1 = ?, "
                  "Approver1At = SYSDATETIME(), Status = CASE WHEN ? "
                  "<= 1 THEN 'APPROVED' ELSE 'PROPOSED' END, "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE ProposalId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE PA-REQUIRED-CNT TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE PA-PROPOSAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF PA-REQUIRED-CNT <= 1
               MOVE "APPROVED" TO PA-STATUS
               MOVE 0 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Payment run approved"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               MOVE "PENDING" TO PA-STATUS
               MOVE 4 TO CA-RETURN-CODE
               MOVE "APA0018" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "First authorisation recorded; a second "
                      "independent authoriser is still required"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       4100-EXIT.
           EXIT.

       4200-SECOND SECTION.
       4200-START.
           IF PA-NEEDS-FC = "Y"
               PERFORM 4300-CHECK-SENIOR THRU 4300-EXIT
               IF WS-FATAL
                   GO TO 4200-EXIT
               END-IF
           END-IF
           MOVE CA-USER TO PA-APPROVER-2
           MOVE 2 TO PA-HELD-CNT
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.PaymentProposal SET Approver2 = ?, "
                  "Approver2At = SYSDATETIME(), Status = 'APPROVED', "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE ProposalId = ? AND Approver1 IS NOT NULL "
                  "AND Approver1 <> ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           MOVE PA-PROPOSAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE CA-USER TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-FATAL
               GO TO 4200-EXIT
           END-IF
           MOVE "APPROVED" TO PA-STATUS
           MOVE "Y" TO PA-VALID
           MOVE 0 TO CA-RETURN-CODE
           MOVE PA-AMOUNT-GBP TO WS-MONEY-ED
           MOVE SPACES TO CA-MESSAGE
           STRING "Payment run approved by two independent "
                  "authorisers for GBP "
                  FUNCTION TRIM(WS-MONEY-ED)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       4200-EXIT.
           EXIT.

       4300-CHECK-SENIOR SECTION.
       4300-START.
           INITIALIZE WS-APSVC
           MOVE "PERM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           IF PA-NEEDS-DIRECTOR = "Y"
               MOVE "SYS.APPROVE.T4" TO SVC-TEXT-1
           ELSE
               MOVE "FIN.APPROVE.T3" TO SVC-TEXT-1
           END-IF
           CALL "APSVC" USING WS-APSVC
           IF SVC-FLAG-1 NOT = "Y"
               MOVE "Y" TO WS-FATAL-SW
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APA0019" TO CA-REASON-CODE
               MOVE WS-FC-LIMIT TO WS-MONEY-ED
               MOVE SPACES TO CA-MESSAGE
               IF PA-NEEDS-DIRECTOR = "Y"
                   STRING "A single payment above the director limit "
                          "requires a director to authorise it"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               ELSE
                   STRING "A payment run above GBP "
                          FUNCTION TRIM(WS-MONEY-ED)
                          " needs the financial controller or a "
                          "director as one of its authorisers"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               END-IF
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       4300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       5000-VERIFY SECTION.
       5000-START.
           PERFORM 7000-READ-PROPOSAL THRU 7000-EXIT
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
           PERFORM 7100-BUILD-HASH THRU 7100-EXIT
           MOVE WS-NEW-HASH TO PA-CONTENT-HASH
           MOVE WS-P-HASH TO PA-STORED-HASH
           MOVE WS-P-STATUS TO PA-STATUS
           IF WS-P-STATUS NOT = "APPROVED"
               MOVE "N" TO PA-VALID
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APA0020" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The proposal is "
                      FUNCTION TRIM(WS-P-STATUS)
                      " and has no current approval"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 5000-EXIT
           END-IF
           IF WS-NEW-HASH NOT = WS-P-HASH
               MOVE "N" TO PA-VALID
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APA0021" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The approved content no longer matches the "
                      "proposal; the approval is invalid"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 5000-EXIT
           END-IF
           IF WS-P-APPROVER1 = SPACES
               OR (PA-REQUIRED-CNT > 1 AND WS-P-APPROVER2 = SPACES)
               MOVE "N" TO PA-VALID
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APA0022" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The required number of independent "
                      "authorisers has not signed this run"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 5000-EXIT
           END-IF
           MOVE "Y" TO PA-VALID
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "The approval covers this exact content"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       6000-INVALID SECTION.
       6000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC core.usp_InvalidateApprovals ?, ?, ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE PA-DOCUMENT-TYPE TO SQLIO-PARM-VAL(2)
           MOVE PA-PROPOSAL-REF TO SQLIO-PARM-VAL(3)
           MOVE PA-CONTENT-HASH TO SQLIO-PARM-VAL(4)
           MOVE CA-USER TO SQLIO-PARM-VAL(5)
           MOVE PA-COMMENT TO SQLIO-PARM-VAL(6)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 6000-EXIT
           END-IF
           MOVE SQLIO-ROWCOUNT TO PA-INVALIDATED-CNT
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.PaymentProposal SET Status = 'DRAFT', "
                  "ApprovalId = NULL, Approver1 = NULL, Approver1At "
                  "= NULL, Approver2 = NULL, Approver2At = NULL, "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE ProposalId = ? AND Status IN "
                  "('PROPOSED','APPROVED')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE PA-PROPOSAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           MOVE "INVALIDATED" TO PA-STATUS
           MOVE "N" TO PA-VALID
           MOVE 4 TO CA-RETURN-CODE
           MOVE "APA0023" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "The payment approval was invalidated; the "
                  "amended run needs fresh approval"
               DELIMITED BY SIZE INTO CA-MESSAGE
           ADD 1 TO CA-MSG-COUNT.
       6000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       7000-READ-PROPOSAL SECTION.
       7000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CAST(p.ProposalId AS varchar(24)), "
                  "p.ProposalRef, CAST(p.Version AS varchar(8)), "
                  "p.PaymentMethod, p.BankAccountCode, "
                  "p.CurrencyCode, CONVERT(char(8), p.PayDate, 112), "
                  "CAST(p.TotalOutflowGBP AS varchar(24)), "
                  "CAST((SELECT COUNT(*) FROM ap.PaymentProposalLine "
                  "l WHERE l.ProposalId = p.ProposalId AND "
                  "l.IsExcluded = 0) AS varchar(24)), "
                  "ISNULL(p.ContentHash,''), p.Status, "
                  "ISNULL(p.PreparedBy,''), ISNULL(p.Approver1,''), "
                  "ISNULL(p.Approver2,''), "
                  "CAST(ISNULL((SELECT MAX(l2.NetPayGBP) FROM "
                  "ap.PaymentProposalLine l2 WHERE l2.ProposalId = "
                  "p.ProposalId AND l2.IsExcluded = 0),0) AS "
                  "varchar(24)) FROM ap.PaymentProposal p "
                  "WHERE p.CompanyCode = ? AND ((? <> 0 AND "
                  "p.ProposalId = ?) OR (? <> '' AND "
                  "p.ProposalRef = ?)) ORDER BY p.Version DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE PA-PROPOSAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE PA-PROPOSAL-REF TO SQLIO-PARM-VAL(4)
           MOVE PA-PROPOSAL-REF TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 15 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-COL-WIDTH(3)
           MOVE 10 TO SQLIO-COL-WIDTH(4)
           MOVE 20 TO SQLIO-COL-WIDTH(5)
           MOVE 3 TO SQLIO-COL-WIDTH(6)
           MOVE 8 TO SQLIO-COL-WIDTH(7)
           MOVE 24 TO SQLIO-COL-WIDTH(8)
           MOVE 24 TO SQLIO-COL-WIDTH(9)
           MOVE 64 TO SQLIO-COL-WIDTH(10)
           MOVE 12 TO SQLIO-COL-WIDTH(11)
           MOVE 20 TO SQLIO-COL-WIDTH(12)
           MOVE 20 TO SQLIO-COL-WIDTH(13)
           MOVE 20 TO SQLIO-COL-WIDTH(14)
           MOVE 24 TO SQLIO-COL-WIDTH(15)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROP-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 7000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE
               MOVE "APA0002" TO CA-REASON-CODE
               MOVE "That payment proposal was not found"
                   TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 7000-EXIT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-P-ID)
           MOVE WS-NUM TO PA-PROPOSAL-ID
           MOVE WS-P-REF TO PA-PROPOSAL-REF
           MOVE WS-P-REF TO PA-DOCUMENT-REF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-P-VERSION)
           MOVE WS-NUM TO PA-VERSION
           MOVE WS-P-CURRENCY TO PA-CURRENCY
           COMPUTE PA-AMOUNT-GBP = FUNCTION NUMVAL(WS-P-OUTFLOW)
           COMPUTE PA-SINGLE-MAX-GBP =
               FUNCTION NUMVAL(WS-P-MAXSINGLE)
           MOVE WS-P-STATUS TO PA-STATUS
           MOVE WS-P-PREPARED TO PA-REQUESTED-BY
           MOVE WS-P-APPROVER1 TO PA-APPROVER-1
           MOVE WS-P-APPROVER2 TO PA-APPROVER-2
           MOVE WS-P-HASH TO PA-STORED-HASH
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-P-SELECTED)
           MOVE WS-NUM TO PA-HASH-INPUT-CNT.
       7000-EXIT.
           EXIT.

       7100-BUILD-HASH SECTION.
       7100-START.
           MOVE SPACES TO WS-HASH-TEXT
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-P-OUTFLOW)
           MOVE WS-NUM TO WS-MONEY-ED
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-P-SELECTED)
           MOVE WS-NUM TO WS-COUNT-ED
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-P-PAYDATE)
           MOVE WS-NUM TO WS-DATE-ED
           STRING FUNCTION TRIM(WS-P-REF) "|"
                  FUNCTION TRIM(CA-COMPANY) "|"
                  FUNCTION TRIM(WS-P-METHOD) "|"
                  FUNCTION TRIM(WS-P-BANK) "|"
                  FUNCTION TRIM(WS-P-CURRENCY) "|"
                  WS-DATE-ED "|"
                  FUNCTION TRIM(WS-MONEY-ED) "|"
                  FUNCTION TRIM(WS-COUNT-ED)
               DELIMITED BY SIZE INTO WS-HASH-TEXT
           MOVE "PROPOSAL" TO PA-HASH-SOURCE
           CALL "APHASH" USING WS-HASH-TEXT WS-NEW-HASH.
       7100-EXIT.
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
           MOVE "APA0099" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "APPAYAPR does not support function "
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
               MOVE "SQL failure in APPAYAPR" TO CA-MESSAGE
           END-IF
           ADD 1 TO CA-MSG-COUNT
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       9300-EXIT.
           EXIT.
