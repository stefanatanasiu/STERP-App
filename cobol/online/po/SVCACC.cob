      *>================================================================
      *> PROGRAM-ID : SVCACC
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : RECORD THE ACCEPTANCE OF A PURCHASED SERVICE OR
      *>              OF A CONTRACT MILESTONE AND ACCRUE ITS COST.  A
      *>              SERVICE HAS NO GOODS INWARD NOTE, SO THE
      *>              EVIDENCE THAT THE WORK WAS DONE IS THE
      *>              ACCEPTANCE ITSELF; THE COST BELONGS TO THE
      *>              PERIOD IN WHICH THE SERVICE WAS DELIVERED AND
      *>              NOT TO THE PERIOD OF THE SUPPLIER'S INVOICE.
      *>              NO STOCK QUANTITY IS EVER CREATED.
      *> CALLED BY  : SERVICE ACCEPTANCE SCREEN, PROJECT WORKBENCH
      *> INTERFACE  : CALL "SVCACC" USING DFHCOMMAREA  (SVCACCCA)
      *> CALLS      : POPOST, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1999-01-26 RJH  CR0851 SERVICE ORDERS AND ACCEPTANCE
      *> 2005-05-04 DJM  CR1207 MILESTONE ACCEPTANCE FOR PROJECTS
      *> 2026-10-06 STERP PO-012, AC-054 WORKED NUMBERS
      *>================================================================
      *> REQ: PO-012, PO-017
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SVCACC.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 26/01/1999.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY POACCNTS.
       COPY POPOSTWK.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
       01  WS-ONE-COL.
           05  WS-C-VALUE              PIC X(60).
       01  WS-TWO-COL.
           05  WS-C2-ONE               PIC X(40).
           05  WS-C2-TWO               PIC X(40).
       01  WS-WORK.
           05  WS-AMT-ED               PIC -(11)9.99.
           05  WS-REMAIN-ED            PIC -(11)9.99.
           05  WS-PCT-ED               PIC -(5)9.9999.
           05  WS-CORR                 PIC X(64).
           05  WS-LINE-ED              PIC -(8)9.
       LINKAGE SECTION.
       COPY SVCACCCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "CALC"
                   PERFORM 2000-VALIDATE THRU 2000-EXIT
               WHEN "ACCEPT"
                   PERFORM 2000-VALIDATE THRU 2000-EXIT
                   IF NOT CA-FAILED
                       PERFORM 3000-ACCEPT THRU 3000-EXIT
                   END-IF
               WHEN "REVERSE"
                   PERFORM 5000-REVERSE THRU 5000-EXIT
               WHEN "ENQUIRE"
                   PERFORM 6000-ENQUIRE THRU 6000-EXIT
               WHEN OTHER
                   MOVE "SVC0006" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                          " is not supported by SVCACC"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   MOVE 8 TO CA-RETURN-CODE
           END-EVALUATE.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INIT SECTION.
       1000-START.
           MOVE "N" TO WS-FATAL-SW
           MOVE "SVCACC" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE 0 TO SA-ACCEPT-GBP SA-ACCRUED-GBP
           MOVE 0 TO SA-REMAIN-COMM-GBP SA-TOTAL-ACC-GBP
           MOVE 0 TO SA-ACCEPTANCE-ID SA-STOCK-QTY
           MOVE SPACES TO SA-JOURNAL-NUMBER
           MOVE "ACCEPTED" TO SA-STATUS
           MOVE "OK" TO SA-OUTCOME
           IF SA-BASIS = SPACES
               MOVE "VALUE" TO SA-BASIS
           END-IF
           IF SA-CURRENCY = SPACES
               MOVE "GBP" TO SA-CURRENCY
           END-IF
           IF SA-FX-RATE = 0
               MOVE 1 TO SA-FX-RATE
           END-IF
           IF SA-ACCEPT-DATE = 0
               MOVE CA-BUSINESS-DATE TO SA-ACCEPT-DATE
           END-IF
           IF SA-POSTING-DATE = 0
               MOVE SA-ACCEPT-DATE TO SA-POSTING-DATE
           END-IF
           IF SA-ACCEPTED-BY = SPACES
               MOVE CA-USER TO SA-ACCEPTED-BY
           END-IF
           IF SA-EXPENSE-ACCOUNT = SPACES
               MOVE PO-ACC-SERVICE TO SA-EXPENSE-ACCOUNT
           END-IF
           IF SA-COST-CENTRE = SPACES
               MOVE "PROD2" TO SA-COST-CENTRE
           END-IF
           IF SA-ORDER-VALUE-GBP = 0 AND SA-ORDER-VALUE NOT = 0
               COMPUTE SA-ORDER-VALUE-GBP ROUNDED =
                       SA-ORDER-VALUE / SA-FX-RATE
           END-IF
           MOVE SPACES TO WS-CORR
           STRING "SVC-" FUNCTION TRIM(CA-COMPANY) "-"
                  FUNCTION TRIM(SA-ACCEPT-REF)
               DELIMITED BY SIZE INTO WS-CORR
           IF CA-CORRELATION-ID NOT = SPACES
               MOVE CA-CORRELATION-ID TO WS-CORR
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> WORK OUT WHAT IS BEING ACCEPTED.  A PERCENTAGE, A QUANTITY,
      *> A MILESTONE VALUE AND AN EXPLICIT VALUE ALL ARRIVE AT THE
      *> SAME ANSWER, AND THE REMAINING COMMITMENT IS WHAT IS LEFT OF
      *> THE APPROVED ORDER VALUE AFTER EVERYTHING ACCEPTED SO FAR.
      *> AC-054: 40% OF 2,000.00 ACCRUES 800.00 AND LEAVES 1,200.00.
      *>----------------------------------------------------------------
       2000-VALIDATE SECTION.
       2000-START.
           IF SA-EVIDENCE = SPACES
               MOVE "SVC0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A service has no delivery note, so the "
                      "evidence that the work was done must be "
                      "recorded with the acceptance"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF SA-BY-MILESTONE AND SA-MILESTONE-REF = SPACES
               MOVE "SVC0005" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A milestone acceptance must name the "
                      "deliverable it signs off"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF SA-REQUESTED-BY NOT = SPACES
              AND SA-REQUESTED-BY = SA-ACCEPTED-BY
               MOVE "SVC0009" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The person who requested the service may not "
                      "also sign it off; acceptance needs a second "
                      "pair of eyes"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           EVALUATE TRUE
               WHEN SA-BY-QUANTITY
                   COMPUTE SA-ACCEPT-VALUE ROUNDED =
                           SA-ACCEPTED-QTY * SA-UNIT-PRICE
               WHEN SA-BY-MILESTONE
                   IF SA-MILESTONE-VALUE NOT = 0
                       MOVE SA-MILESTONE-VALUE TO SA-ACCEPT-VALUE
                   END-IF
               WHEN OTHER
                   IF SA-ACCEPT-VALUE = 0 AND SA-ACCEPT-PCT NOT = 0
                       COMPUTE SA-ACCEPT-VALUE ROUNDED =
                               SA-ORDER-VALUE * SA-ACCEPT-PCT / 100
                   END-IF
           END-EVALUATE
           COMPUTE SA-ACCEPT-GBP ROUNDED =
                   SA-ACCEPT-VALUE / SA-FX-RATE
           IF SA-ACCEPT-GBP <= 0
               MOVE "SVC0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The accepted value is zero; there is nothing "
                      "to accrue"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF SA-ALREADY-ACC-GBP = 0 AND CA-FUNCTION NOT = "CALC"
               PERFORM 1500-READ-ACCEPTED THRU 1500-EXIT
           END-IF
           COMPUTE SA-TOTAL-ACC-GBP =
                   SA-ALREADY-ACC-GBP + SA-ACCEPT-GBP
           COMPUTE SA-REMAIN-COMM-GBP =
                   SA-ORDER-VALUE-GBP - SA-TOTAL-ACC-GBP
           IF SA-ORDER-VALUE-GBP > 0 AND SA-REMAIN-COMM-GBP < 0
               MOVE "SVC0003" TO CA-REASON-CODE
               COMPUTE SA-REMAIN-COMM-GBP =
                       SA-ORDER-VALUE-GBP - SA-ALREADY-ACC-GBP
               MOVE SA-REMAIN-COMM-GBP TO WS-AMT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "Only " FUNCTION TRIM(WS-AMT-ED)
                      " of the approved service order is left to "
                      "accept; excess service billing is an "
                      "exception and is never accrued silently"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 0 TO SA-ACCEPT-GBP SA-TOTAL-ACC-GBP
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1500-READ-ACCEPTED SECTION.
       1500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(SUM(AcceptedValueGBP),0) AS "
                  "varchar(40)) FROM po.ServiceAcceptance WHERE "
                  "CompanyCode = ? AND PurchaseOrderNumber = ? AND "
                  "PurchaseOrderLine = ? AND Status = 'ACCEPTED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SA-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE SA-ORDER-LINE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 60 TO SQLIO-COL-WIDTH(1)
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-SQLCODE = 0 AND WS-C-VALUE NOT = SPACES
               MOVE FUNCTION NUMVAL(WS-C-VALUE)
                 TO SA-ALREADY-ACC-GBP
           END-IF.
       1500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> ACCEPTANCE.  THE ROW IS WRITTEN FIRST SO THAT THE EVIDENCE
      *> SURVIVES A POSTING FAILURE, THEN THE ACCRUAL IS POSTED.  THE
      *> SUPPLIER'S INVOICE LATER CLEARS THIS ACCRUAL; IT MUST NOT
      *> RECOGNISE THE EXPENSE A SECOND TIME (PO-017).
      *>----------------------------------------------------------------
       3000-ACCEPT SECTION.
       3000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.ServiceAcceptance (CompanyCode, "
                  "AcceptanceRef, PurchaseOrderNumber, "
                  "PurchaseOrderLine, SupplierCode, "
                  "AcceptanceBasis, AcceptedQty, MilestoneRef, "
                  "AcceptedValue, AcceptedValueGBP, "
                  "RemainingCommitmentGBP, AcceptanceDate, "
                  "PostingDate, Evidence, AcceptedBy, CreatedBy) "
                  "OUTPUT INSERTED.AcceptanceId VALUES "
                  "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SA-ACCEPT-REF TO SQLIO-PARM-VAL(2)
           MOVE SA-ORDER-NUMBER TO SQLIO-PARM-VAL(3)
           MOVE SA-ORDER-LINE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE SA-SUPPLIER TO SQLIO-PARM-VAL(5)
           MOVE SA-BASIS TO SQLIO-PARM-VAL(6)
           MOVE SA-ACCEPTED-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE SA-MILESTONE-REF TO SQLIO-PARM-VAL(8)
           IF SA-MILESTONE-REF = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(8)
           END-IF
           MOVE SA-ACCEPT-VALUE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE SA-ACCEPT-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE SA-REMAIN-COMM-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE SA-ACCEPT-DATE TO SQLIO-PARM-VAL(12)
           MOVE SA-POSTING-DATE TO SQLIO-PARM-VAL(13)
           MOVE SA-EVIDENCE TO SQLIO-PARM-VAL(14)
           MOVE SA-ACCEPTED-BY TO SQLIO-PARM-VAL(15)
           MOVE CA-USER TO SQLIO-PARM-VAL(16)
           MOVE 16 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-SQLCODE = 0 AND WS-C-VALUE NOT = SPACES
               MOVE FUNCTION NUMVAL(WS-C-VALUE) TO SA-ACCEPTANCE-ID
           END-IF
           PERFORM 3500-POST THRU 3500-EXIT.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3500-POST SECTION.
       3500-START.
           INITIALIZE WS-POPOST
           MOVE "POST" TO PP-FUNC
           MOVE CA-COMPANY TO PP-COMPANY
           MOVE CA-SITE TO PP-SITE
           MOVE CA-USER TO PP-USER
           MOVE CA-BUSINESS-DATE TO PP-BUSINESS-DATE
           MOVE WS-CORR TO PP-CORRELATION
           MOVE SA-POSTING-DATE TO PP-POSTING-DATE
           MOVE SA-ACCEPT-DATE TO PP-DOCUMENT-DATE
           MOVE "GBP" TO PP-CURRENCY
           MOVE 1 TO PP-FX-RATE
           MOVE PO-EV-SERVICE TO PP-EVENT-CODE
           MOVE SA-ACCEPT-REF TO PP-SOURCE-REF
           MOVE "PO" TO PP-SUBLEDGER-TYPE
           MOVE SA-SUPPLIER TO PP-SUBLEDGER-REF
           MOVE SA-EVIDENCE TO PP-EVIDENCE-REF
           MOVE SPACES TO PP-DESCRIPTION
           STRING "Service accepted " FUNCTION TRIM(SA-ACCEPT-REF)
                  " on order " FUNCTION TRIM(SA-ORDER-NUMBER)
                  " - " FUNCTION TRIM(SA-DESCRIPTION)
               DELIMITED BY SIZE INTO PP-DESCRIPTION
           MOVE SA-EXPENSE-ACCOUNT TO PP-L-ACCOUNT(1)
           MOVE SA-ACCEPT-GBP TO PP-L-DR(1)
           MOVE 0 TO PP-L-CR(1)
           MOVE SA-COST-CENTRE TO PP-L-COST-CTR(1)
           MOVE SA-PROJECT TO PP-L-PROJECT(1)
           MOVE SA-JOB-REF TO PP-L-JOB(1)
           MOVE "Service delivered and accepted"
             TO PP-L-NARRATIVE(1)
           MOVE PO-ACC-ACCRUAL TO PP-L-ACCOUNT(2)
           MOVE 0 TO PP-L-DR(2)
           MOVE SA-ACCEPT-GBP TO PP-L-CR(2)
           MOVE "Accrued service cost, not yet invoiced"
             TO PP-L-NARRATIVE(2)
           MOVE 2 TO PP-LINE-CNT
           CALL "POPOST" USING WS-POPOST
           MOVE PP-JOURNAL-NUMBER TO SA-JOURNAL-NUMBER
           IF PP-RC > 4
               MOVE PP-RC TO CA-RETURN-CODE
               MOVE "SVC0007" TO CA-REASON-CODE
               MOVE PP-MESSAGE TO CA-MESSAGE
               MOVE 0 TO SA-ACCRUED-GBP
           ELSE
               MOVE SA-ACCEPT-GBP TO SA-ACCRUED-GBP
               MOVE SA-ACCEPT-GBP TO WS-AMT-ED
               MOVE SA-REMAIN-COMM-GBP TO WS-REMAIN-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "Accepted and accrued "
                      FUNCTION TRIM(WS-AMT-ED)
                      "; remaining commitment "
                      FUNCTION TRIM(WS-REMAIN-ED)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       3500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> REVERSAL.  AN ACCEPTANCE SIGNED IN ERROR IS REVERSED, NOT
      *> DELETED: THE ORIGINAL AND ITS REVERSAL BOTH REMAIN VISIBLE
      *> SO THAT THE AUDIT TRAIL STILL SHOWS WHO SIGNED WHAT.
      *>----------------------------------------------------------------
       5000-REVERSE SECTION.
       5000-START.
           IF SA-REVERSE-REASON = SPACES
               MOVE "SVC0008" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A reversal of a service acceptance must say "
                      "why the service was not in fact delivered"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(AcceptedValueGBP AS varchar(40)), "
                  "SupplierCode FROM po.ServiceAcceptance WHERE "
                  "CompanyCode = ? AND AcceptanceRef = ? AND "
                  "Status = 'ACCEPTED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SA-REVERSE-REF TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO-COL
           IF SQLIO-SQLCODE NOT = 0 OR WS-C2-ONE = SPACES
               MOVE 16 TO CA-RETURN-CODE
               MOVE "SVC0008" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Acceptance " FUNCTION TRIM(SA-REVERSE-REF)
                      " was not found, or it has already been "
                      "reversed"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 5000-EXIT
           END-IF
           MOVE FUNCTION NUMVAL(WS-C2-ONE) TO SA-ACCEPT-GBP
           MOVE WS-C2-TWO TO SA-SUPPLIER
           INITIALIZE WS-POPOST
           MOVE "POST" TO PP-FUNC
           MOVE CA-COMPANY TO PP-COMPANY
           MOVE CA-SITE TO PP-SITE
           MOVE CA-USER TO PP-USER
           MOVE CA-BUSINESS-DATE TO PP-BUSINESS-DATE
           MOVE SPACES TO PP-CORRELATION
           STRING "SVCREV-" FUNCTION TRIM(CA-COMPANY) "-"
                  FUNCTION TRIM(SA-REVERSE-REF)
               DELIMITED BY SIZE INTO PP-CORRELATION
           MOVE CA-BUSINESS-DATE TO PP-POSTING-DATE
           MOVE CA-BUSINESS-DATE TO PP-DOCUMENT-DATE
           MOVE "GBP" TO PP-CURRENCY
           MOVE 1 TO PP-FX-RATE
           MOVE PO-EV-SERVICE TO PP-EVENT-CODE
           MOVE SA-REVERSE-REF TO PP-SOURCE-REF
           MOVE "PO" TO PP-SUBLEDGER-TYPE
           MOVE SA-SUPPLIER TO PP-SUBLEDGER-REF
           MOVE SPACES TO PP-DESCRIPTION
           STRING "Reversal of service acceptance "
                  FUNCTION TRIM(SA-REVERSE-REF) " - "
                  FUNCTION TRIM(SA-REVERSE-REASON)
               DELIMITED BY SIZE INTO PP-DESCRIPTION
           MOVE PO-ACC-ACCRUAL TO PP-L-ACCOUNT(1)
           MOVE SA-ACCEPT-GBP TO PP-L-DR(1)
           MOVE 0 TO PP-L-CR(1)
           MOVE "Service accrual reversed" TO PP-L-NARRATIVE(1)
           MOVE SA-EXPENSE-ACCOUNT TO PP-L-ACCOUNT(2)
           MOVE 0 TO PP-L-DR(2)
           MOVE SA-ACCEPT-GBP TO PP-L-CR(2)
           MOVE SA-COST-CENTRE TO PP-L-COST-CTR(2)
           MOVE "Service expense reversed" TO PP-L-NARRATIVE(2)
           MOVE 2 TO PP-LINE-CNT
           CALL "POPOST" USING WS-POPOST
           MOVE PP-JOURNAL-NUMBER TO SA-JOURNAL-NUMBER
           IF PP-RC > 4
               MOVE PP-RC TO CA-RETURN-CODE
               MOVE "SVC0007" TO CA-REASON-CODE
               MOVE PP-MESSAGE TO CA-MESSAGE
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE po.ServiceAcceptance SET Status = "
                  "'REVERSED' WHERE CompanyCode = ? AND "
                  "AcceptanceRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SA-REVERSE-REF TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           MOVE "REVERSED" TO SA-STATUS.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       6000-ENQUIRE SECTION.
       6000-START.
           PERFORM 1500-READ-ACCEPTED THRU 1500-EXIT
           MOVE SA-ALREADY-ACC-GBP TO SA-TOTAL-ACC-GBP
           COMPUTE SA-REMAIN-COMM-GBP =
                   SA-ORDER-VALUE-GBP - SA-TOTAL-ACC-GBP
           IF SA-REMAIN-COMM-GBP < 0
               MOVE 0 TO SA-REMAIN-COMM-GBP
           END-IF
           MOVE 0 TO SA-STOCK-QTY.
       6000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE 8 TO CA-RETURN-CODE
           MOVE "Y" TO WS-FATAL-SW
           MOVE "REJECTED" TO SA-OUTCOME.
       9000-EXIT.
           EXIT.
       END PROGRAM SVCACC.
