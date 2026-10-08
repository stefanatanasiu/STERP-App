      *>================================================================
      *> PROGRAM-ID : DISHONOR
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : DISHONOURED AND REVERSED CUSTOMER RECEIPTS.  THE
      *>              DEBT IS REINSTATED FOR EXACTLY THE AMOUNT OF THE
      *>              DISHONOURED RECEIPT, EVERY ALLOCATION THAT USED
      *>              IT IS REVERSED ACROSS ALL AFFECTED ITEMS, AND
      *>              ANY BANK FEE IS A SEPARATE CHARGE.  OTHER
      *>              CONTRIBUTIONS TO THE SAME INVOICE - A SECOND
      *>              RECEIPT, A DISCOUNT OR AN APPROVED WRITE OFF -
      *>              KEEP THEIR OWN STATUS UNLESS SEPARATELY
      *>              REVERSED.  ARUDD UNPAIDS AND DDICA INDEMNITY
      *>              CLAIMS ARRIVE THE SAME WAY (AR-025).
      *> CALLED BY  : BANK RECONCILIATION, DDCOLL, RECEIPT ENQUIRY
      *> INTERFACE  : CALL "DISHONOR" USING WS-DISHONOR-CA
      *>              COPYBOOK DISHONORCA
      *> CALLS      : SQLIO, PERMCHK, PERIODCK, ARPOSTGL, EXCPRAIS,
      *>              CREDCHK
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-06-11 RJH  ORIGINAL UNPAID CHEQUE HANDLING (MVS)
      *> 1997-10-02 DJM  CR0889 BACS UNPAID RETURN FILE
      *> 2026-10-05 STERP ARUDD / DDICA AND EXPOSURE REASSESSMENT
      *> 2026-10-07 STERP CR2613 EACH REINSTATED ITEM REPORTS THE OPEN
      *>                 AMOUNT LEFT ON IT AFTER THE DISHONOUR
      *>================================================================
      *> REQ: AR-014, AR-025
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISHONOR.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 11/06/1991.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ARWORK.
       COPY PERMCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-PERM-CA==.
       COPY PERIODCKCA REPLACING ==DFHCOMMAREA== BY ==WS-PERIOD-CA==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXCP-CA==.
       COPY CREDCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-CRED-CA==.
       COPY ARPOSTGLCA REPLACING ==DFHCOMMAREA== BY ==WS-POSTGL-CA==.
       01  WS-RCPT.
           05  WS-R-ID                 PIC X(24).
           05  WS-R-CUSTOMER           PIC X(20).
           05  WS-R-CURRENCY           PIC X(3).
           05  WS-R-GROSS              PIC X(24).
           05  WS-R-GROSS-GBP          PIC X(24).
           05  WS-R-UNALLOC            PIC X(24).
           05  WS-R-STATE              PIC X(12).
           05  WS-R-BANK               PIC X(20).
           05  WS-R-NUMBER             PIC X(30).
           05  WS-R-ITEM-ID            PIC X(24).
       01  WS-ALLOC.
           05  WS-A-ID                 PIC X(24).
           05  WS-A-TARGET             PIC X(24).
           05  WS-A-DOCNO              PIC X(30).
           05  WS-A-APPLIED            PIC X(24).
           05  WS-A-APPLIED-GBP        PIC X(24).
           05  WS-A-RESID              PIC X(24).
      *>   the target open item behind each reported line, so the
      *>   residual can be re-read once the reversal is committed
       01  WS-TGT-TAB.
           05  WS-TGT                  OCCURS 20 TIMES PIC X(24).
       01  WS-RES-ROW.
           05  WS-RES-AMT              PIC X(24).
           05  WS-RES-SPARE            PIC X(24).
       01  WS-ID-ROW.
           05  WS-NEW-ID               PIC X(24).
           05  WS-ID2                  PIC X(24).
       01  WS-SW.
           05  WS-TRAN-OPEN            PIC X VALUE "N".
           05  WS-MORE                 PIC X VALUE "Y".
       LINKAGE SECTION.
       COPY DISHONORCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF AR-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0300-READ-RECEIPT
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0400-LIST-ALLOCATIONS
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE AR-FUNC
               WHEN "ASSESS"   PERFORM 0500-ASSESS-ONLY
               WHEN "DISHONR"  PERFORM 1000-DISHONOUR
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           PERFORM 9900-SET-RESULT
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE AR-WORK
           INITIALIZE AR-CTL
           INITIALIZE WS-RCPT
           INITIALIZE WS-ALLOC
           MOVE "N" TO WS-TRAN-OPEN
           MOVE CA-FUNCTION OF DFHCOMMAREA TO AR-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO AR-CO
           MOVE CA-SITE OF DFHCOMMAREA TO AR-SITE
           MOVE CA-USER OF DFHCOMMAREA TO AR-USERID
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO AR-BUSDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO AR-CORREL
           MOVE "DISHONOR" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO AR-RC AR-MSG-CNT
           MOVE SPACES TO AR-REASON AR-MSG
           MOVE "AR-014" TO AR-REQ-REF
           MOVE 0 TO DH-REINSTATED DH-ALLOC-REVERSED DH-ITEM-CNT
           MOVE 0 TO DH-UNAPPLIED-BACK DH-EXPOSURE-AFTER
           MOVE SPACES TO WS-TGT-TAB
           IF DH-DISHONOUR-DATE = 0
               MOVE AR-BUSDATE TO DH-DISHONOUR-DATE
           END-IF
           IF DH-POSTING-DATE = 0
               MOVE DH-DISHONOUR-DATE TO DH-POSTING-DATE
           END-IF
           IF DH-SOURCE = SPACES
               MOVE "BANK" TO DH-SOURCE
           END-IF.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF AR-CO = SPACES
               MOVE "DIS0001" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A company code is required" DELIMITED BY SIZE
                   INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF DH-RECEIPT-NO = SPACES AND DH-RECEIPT-ID = 0
               MOVE "DIS0002" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A receipt reference is required" DELIMITED BY
                   SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AR-FUNC = "DISHONR" AND DH-REASON = SPACES
               MOVE "DIS0003" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A dishonour needs a stated reason"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

       0300-READ-RECEIPT SECTION.
       0300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), r.ReceiptId), "
                  "ISNULL(r.CustomerCode,''), r.CurrencyCode, "
                  "CONVERT(varchar(24), r.GrossReceived), "
                  "CONVERT(varchar(24), r.GrossReceivedGBP), "
                  "CONVERT(varchar(24), r.UnallocatedAmount), "
                  "r.ConfirmationState, r.BankAccountCode, "
                  "r.ReceiptNumber, CONVERT(varchar(24), "
                  "ISNULL((SELECT TOP (1) o.OpenItemId FROM "
                  "ar.OpenItem o WHERE o.SourceReceiptId = "
                  "r.ReceiptId),0)) FROM ar.Receipt r "
                  "WHERE r.CompanyCode = ? AND (? = '' OR "
                  "r.ReceiptNumber = ?) AND (r.ReceiptId = "
                  "CASE WHEN ? = '0' THEN r.ReceiptId ELSE "
                  "CONVERT(bigint, ?) END)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE DH-RECEIPT-NO TO SQLIO-PARM-VAL(2)
           MOVE DH-RECEIPT-NO TO SQLIO-PARM-VAL(3)
           MOVE DH-RECEIPT-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(5)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 5
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE 10 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 3 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 24 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 12 TO SQLIO-COL-WIDTH(7)
           MOVE 20 TO SQLIO-COL-WIDTH(8)
           MOVE 30 TO SQLIO-COL-WIDTH(9)
           MOVE 24 TO SQLIO-COL-WIDTH(10)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-RCPT
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "DIS0004" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "That receipt is not held for this company"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 0300-EXIT
           END-IF
           COMPUTE DH-RECEIPT-ID = FUNCTION NUMVAL(WS-R-ID)
           MOVE WS-R-NUMBER TO DH-RECEIPT-NO
           MOVE WS-R-CUSTOMER TO DH-CUSTOMER
           COMPUTE DH-RECEIPT-GBP = FUNCTION NUMVAL(WS-R-GROSS-GBP)
           IF WS-R-STATE = "DISHONOURED" OR WS-R-STATE = "REVERSED"
               MOVE 8 TO AR-RC
               MOVE "DIS0005" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Receipt " FUNCTION TRIM(DH-RECEIPT-NO)
                      " has already been reversed"
                   DELIMITED BY SIZE INTO AR-MSG
           END-IF.
       0300-EXIT.
           EXIT.

      *>   every allocation this receipt took part in
       0400-LIST-ALLOCATIONS SECTION.
       0400-START.
           MOVE 0 TO DH-ITEM-CNT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), a.AllocationId), "
                  "CONVERT(varchar(24), a.TargetOpenItemId), "
                  "t.DocumentNumber, CONVERT(varchar(24), "
                  "a.AppliedAmount), CONVERT(varchar(24), "
                  "a.AppliedAmountGBP), CONVERT(varchar(24), "
                  "t.ResidualAmount) FROM ar.Allocation a "
                  "JOIN ar.OpenItem s ON s.OpenItemId = "
                  "a.SourceOpenItemId JOIN ar.OpenItem t ON "
                  "t.OpenItemId = a.TargetOpenItemId "
                  "WHERE a.CompanyCode = ? AND a.Status = 'ACTIVE' "
                  "AND s.SourceReceiptId = ? ORDER BY a.AllocationId"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE DH-RECEIPT-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 30 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 24 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ALLOC
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0400-EXIT
           END-IF
           MOVE "Y" TO WS-MORE
           PERFORM UNTIL WS-MORE = "N"
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ALLOC
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "N" TO WS-MORE
               ELSE
                   IF DH-ITEM-CNT < 20
                       ADD 1 TO DH-ITEM-CNT
                       MOVE WS-A-DOCNO TO DH-I-DOC-NO(DH-ITEM-CNT)
                       MOVE WS-A-TARGET TO WS-TGT(DH-ITEM-CNT)
                       COMPUTE DH-I-AMOUNT(DH-ITEM-CNT) =
                           FUNCTION NUMVAL(WS-A-APPLIED)
      *>                 what the item is left owing once the
      *>                 allocation is taken off it again (AR-014);
      *>                 other contributions keep their own effect
                       COMPUTE DH-I-RESID-AFTER(DH-ITEM-CNT) =
                           FUNCTION NUMVAL(WS-A-RESID)
                           + FUNCTION NUMVAL(WS-A-APPLIED)
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ALLOC.
       0400-EXIT.
           EXIT.

       0500-ASSESS-ONLY SECTION.
       0500-START.
           MOVE DH-RECEIPT-GBP TO DH-REINSTATED
           MOVE DH-ITEM-CNT TO DH-ALLOC-REVERSED
           MOVE DH-RECEIPT-GBP TO AR-MONEY-UK
           MOVE DH-ITEM-CNT TO AR-INT-ED
           MOVE SPACES TO AR-MSG
           STRING "Dishonouring " FUNCTION TRIM(DH-RECEIPT-NO)
                  " would reinstate GBP "
                  FUNCTION TRIM(AR-MONEY-UK) " across "
                  FUNCTION TRIM(AR-INT-ED) " allocation(s)"
               DELIMITED BY SIZE INTO AR-MSG.
       0500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-DISHONOUR SECTION.
       1000-START.
           PERFORM 1050-PERMISSION
           IF AR-RC-BAD
               GO TO 1000-EXIT
           END-IF
           PERFORM 1100-PERIOD
           IF AR-RC-BAD
               GO TO 1000-EXIT
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "Y" TO WS-TRAN-OPEN
           PERFORM 1200-REVERSE-ALLOCATIONS
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           PERFORM 1300-MARK-RECEIPT
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           PERFORM 1400-POST-REVERSAL
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           IF DH-BANK-FEE > 0
               PERFORM 1500-POST-BANK-FEE
               IF AR-RC-BAD
                   PERFORM 9500-ROLLBACK
                   GO TO 1000-EXIT
               END-IF
           END-IF
           MOVE "COMT" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "N" TO WS-TRAN-OPEN
           PERFORM 1250-REFRESH-RESIDUALS
           PERFORM 1600-REASSESS-EXPOSURE
           PERFORM 1700-RAISE-EXCEPTION
           MOVE DH-RECEIPT-GBP TO DH-REINSTATED
           MOVE DH-RECEIPT-GBP TO AR-MONEY-UK
           MOVE DH-ALLOC-REVERSED TO AR-INT-ED
           MOVE SPACES TO AR-MSG
           STRING "Receipt " FUNCTION TRIM(DH-RECEIPT-NO)
                  " dishonoured; GBP " FUNCTION TRIM(AR-MONEY-UK)
                  " reinstated across " FUNCTION TRIM(AR-INT-ED)
                  " allocation(s)"
               DELIMITED BY SIZE INTO AR-MSG.
       1000-EXIT.
           EXIT.

       1050-PERMISSION SECTION.
       1050-START.
           INITIALIZE WS-PERM-CA
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PERM-CA
           MOVE AR-SITE TO CA-SITE OF WS-PERM-CA
           MOVE AR-USERID TO CA-USER OF WS-PERM-CA
           MOVE DH-POSTING-DATE TO CA-BUSINESS-DATE OF WS-PERM-CA
           MOVE "AR.RECEIPT.POST" TO PK-PERMISSION
           CALL "PERMCHK" USING WS-PERM-CA
           IF CA-RETURN-CODE OF WS-PERM-CA > 4
               MOVE 6 TO AR-RC
               MOVE "DIS0006" TO AR-REASON
               MOVE CA-MESSAGE OF WS-PERM-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       1050-EXIT.
           EXIT.

       1100-PERIOD SECTION.
       1100-START.
           INITIALIZE WS-PERIOD-CA
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERIOD-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PERIOD-CA
           MOVE AR-USERID TO CA-USER OF WS-PERIOD-CA
           MOVE DH-POSTING-DATE TO CA-BUSINESS-DATE OF WS-PERIOD-CA
           MOVE DH-POSTING-DATE TO PC-POSTING-DATE
           MOVE DH-DISHONOUR-DATE TO PC-DOCUMENT-DATE
           MOVE "POST" TO PC-ACTION
           CALL "PERIODCK" USING WS-PERIOD-CA
           IF CA-RETURN-CODE OF WS-PERIOD-CA > 4
               MOVE CA-RETURN-CODE OF WS-PERIOD-CA TO AR-RC
               MOVE CA-REASON-CODE OF WS-PERIOD-CA TO AR-REASON
               MOVE CA-MESSAGE OF WS-PERIOD-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       1100-EXIT.
           EXIT.

      *>   every allocation that used this receipt is reversed; other
      *>   contributions to the same invoice are untouched
       1200-REVERSE-ALLOCATIONS SECTION.
       1200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; "
                  "UPDATE t SET t.ResidualAmount = t.ResidualAmount "
                  "+ a.AppliedAmount, t.ResidualAmountGBP = "
                  "t.ResidualAmountGBP + a.AppliedAmountGBP, "
                  "t.SettlementStatus = 'OPEN', t.ModifiedBy = ?, "
                  "t.ModifiedAt = SYSDATETIME() "
                  "FROM ar.OpenItem t JOIN ar.Allocation a ON "
                  "a.TargetOpenItemId = t.OpenItemId "
                  "JOIN ar.OpenItem s ON s.OpenItemId = "
                  "a.SourceOpenItemId WHERE a.CompanyCode = ? "
                  "AND a.Status = 'ACTIVE' AND s.SourceReceiptId = ?;"
                  " UPDATE a SET a.Status = 'REVERSED', "
                  "a.ReversedAt = SYSDATETIME(), a.ReversedBy = ? "
                  "FROM ar.Allocation a JOIN ar.OpenItem s ON "
                  "s.OpenItemId = a.SourceOpenItemId "
                  "WHERE a.CompanyCode = ? AND a.Status = 'ACTIVE' "
                  "AND s.SourceReceiptId = ?; "
                  "SELECT CONVERT(varchar(24), @@ROWCOUNT), ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE AR-USERID TO SQLIO-PARM-VAL(1)
           MOVE AR-CO TO SQLIO-PARM-VAL(2)
           MOVE DH-RECEIPT-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE AR-USERID TO SQLIO-PARM-VAL(4)
           MOVE AR-CO TO SQLIO-PARM-VAL(5)
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(6)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 6
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1200-EXIT
           END-IF
           COMPUTE DH-ALLOC-REVERSED = FUNCTION NUMVAL(WS-NEW-ID).
       1200-EXIT.
           EXIT.

      *>   after the commit each reinstated item is re-read so the
      *>   caller is told the open amount the customer really owes
      *>   on it now, not a projection (AR-014)
       1250-REFRESH-RESIDUALS SECTION.
       1250-START.
           IF DH-ITEM-CNT = 0
               GO TO 1250-EXIT
           END-IF
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > DH-ITEM-CNT
               PERFORM 1260-ONE-RESIDUAL
           END-PERFORM.
       1250-EXIT.
           EXIT.

       1260-ONE-RESIDUAL SECTION.
       1260-START.
           IF WS-TGT(AR-I) = SPACES
               GO TO 1260-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), o.ResidualAmount), "
                  "o.SettlementStatus FROM ar.OpenItem o "
                  "WHERE o.CompanyCode = ? AND o.OpenItemId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE FUNCTION TRIM(WS-TGT(AR-I)) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-RES-ROW
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               GO TO 1260-EXIT
           END-IF
           COMPUTE DH-I-RESID-AFTER(AR-I) =
               FUNCTION NUMVAL(WS-RES-AMT).
       1260-EXIT.
           EXIT.

       1300-MARK-RECEIPT SECTION.
       1300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.Receipt SET ConfirmationState = "
                  "'DISHONOURED', DishonouredDate = ?, "
                  "DishonourReason = ?, AllocatedAmount = 0, "
                  "UnallocatedAmount = 0, Status = 'REVERSED', "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE ReceiptId = ?; "
                  "UPDATE ar.OpenItem SET ResidualAmount = 0, "
                  "ResidualAmountGBP = 0, SettlementStatus = "
                  "'REVERSED', ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() WHERE SourceReceiptId = ?; "
                  "UPDATE ar.DirectDebitCollection SET Status = "
                  "CASE WHEN ? = 'DDICA' THEN 'INDEMNITY' ELSE "
                  "'UNPAID' END, UnpaidReasonCode = NULLIF(?,''), "
                  "UnpaidDate = ?, IndemnityClaimRef = NULLIF(?,'') "
                  "WHERE CompanyCode = ? AND ReceiptId = ? "
                  "AND ? <> 'BANK'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 12 TO SQLIO-PARM-CNT
           MOVE DH-DISHONOUR-DATE TO SQLIO-PARM-VAL(1)
           MOVE DH-REASON TO SQLIO-PARM-VAL(2)
           MOVE AR-USERID TO SQLIO-PARM-VAL(3)
           MOVE DH-RECEIPT-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE AR-USERID TO SQLIO-PARM-VAL(5)
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(6)
           MOVE DH-SOURCE TO SQLIO-PARM-VAL(7)
           MOVE DH-UNPAID-CODE TO SQLIO-PARM-VAL(8)
           MOVE DH-DISHONOUR-DATE TO SQLIO-PARM-VAL(9)
           MOVE DH-INDEMNITY-REF TO SQLIO-PARM-VAL(10)
           MOVE AR-CO TO SQLIO-PARM-VAL(11)
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(12)
           MOVE DH-SOURCE TO SQLIO-PARM-VAL(13)
           MOVE 13 TO SQLIO-PARM-CNT
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 13
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1300-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW.
       1300-EXIT.
           EXIT.

      *>   DR trade debtors, CR bank - the mirror of the receipt
       1400-POST-REVERSAL SECTION.
       1400-START.
           INITIALIZE WS-POSTGL-CA
           MOVE "POST" TO CA-FUNCTION OF WS-POSTGL-CA
           MOVE AR-CO TO CA-COMPANY OF WS-POSTGL-CA
           MOVE AR-USERID TO CA-USER OF WS-POSTGL-CA
           MOVE SPACES TO AR-SCRATCH
           STRING FUNCTION TRIM(AR-CORREL) "-DIS"
               DELIMITED BY SIZE INTO AR-SCRATCH
           MOVE AR-SCRATCH TO CA-CORRELATION-ID OF WS-POSTGL-CA
           MOVE DH-POSTING-DATE TO CA-BUSINESS-DATE OF WS-POSTGL-CA
           MOVE "AR.FXLOSS" TO PG-EVENT-CODE
           MOVE "AR.RECEIPT" TO PG-EVENT-CODE
           MOVE "AR" TO PG-JOURNAL-TYPE
           MOVE "AR" TO PG-SOURCE-MODULE
           MOVE DH-RECEIPT-NO TO PG-SOURCE-REF
           MOVE DH-DISHONOUR-DATE TO PG-DOC-DATE
           MOVE DH-POSTING-DATE TO PG-POSTING-DATE
           MOVE "GBP" TO PG-CURRENCY
           MOVE 1 TO PG-FX-RATE
           MOVE "N" TO PG-OWN-TRAN
           MOVE SPACES TO PG-DESCRIPTION
           STRING "Dishonoured receipt "
                  FUNCTION TRIM(DH-RECEIPT-NO) " - "
                  FUNCTION TRIM(DH-SOURCE)
               DELIMITED BY SIZE INTO PG-DESCRIPTION
           MOVE 2 TO PG-LEG-CNT
           MOVE "DEBTOR" TO PG-G-LEG-CODE(1)
           MOVE DH-RECEIPT-GBP TO PG-G-DEBIT(1)
           MOVE "AR" TO PG-G-SUBLEDGER(1)
           MOVE DH-CUSTOMER TO PG-G-SUBREF(1)
           MOVE DH-REASON TO PG-G-NARRATIVE(1)
           MOVE "BANK" TO PG-G-LEG-CODE(2)
           MOVE DH-RECEIPT-GBP TO PG-G-CREDIT(2)
           MOVE WS-R-BANK TO PG-G-BANK-ACCT(2)
           MOVE DH-RECEIPT-NO TO PG-G-NARRATIVE(2)
           CALL "ARPOSTGL" USING WS-POSTGL-CA
           IF CA-RETURN-CODE OF WS-POSTGL-CA > 4
               MOVE CA-RETURN-CODE OF WS-POSTGL-CA TO AR-RC
               MOVE CA-REASON-CODE OF WS-POSTGL-CA TO AR-REASON
               MOVE CA-MESSAGE OF WS-POSTGL-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 1400-EXIT
           END-IF
           MOVE PG-JOURNAL-NO TO DH-JOURNAL-NO.
       1400-EXIT.
           EXIT.

      *>   the bank fee is a separate charge, never netted off the
      *>   customer's settlement
       1500-POST-BANK-FEE SECTION.
       1500-START.
           INITIALIZE WS-POSTGL-CA
           MOVE "POST" TO CA-FUNCTION OF WS-POSTGL-CA
           MOVE AR-CO TO CA-COMPANY OF WS-POSTGL-CA
           MOVE AR-USERID TO CA-USER OF WS-POSTGL-CA
           MOVE SPACES TO AR-SCRATCH
           STRING FUNCTION TRIM(AR-CORREL) "-FEE"
               DELIMITED BY SIZE INTO AR-SCRATCH
           MOVE AR-SCRATCH TO CA-CORRELATION-ID OF WS-POSTGL-CA
           MOVE DH-POSTING-DATE TO CA-BUSINESS-DATE OF WS-POSTGL-CA
           MOVE "AR.ROUNDING" TO PG-EVENT-CODE
           MOVE "AR" TO PG-JOURNAL-TYPE
           MOVE "AR" TO PG-SOURCE-MODULE
           MOVE DH-RECEIPT-NO TO PG-SOURCE-REF
           MOVE DH-POSTING-DATE TO PG-DOC-DATE
           MOVE DH-POSTING-DATE TO PG-POSTING-DATE
           MOVE "GBP" TO PG-CURRENCY
           MOVE 1 TO PG-FX-RATE
           MOVE "N" TO PG-OWN-TRAN
           MOVE SPACES TO PG-DESCRIPTION
           STRING "Bank charge on dishonoured receipt "
                  FUNCTION TRIM(DH-RECEIPT-NO)
               DELIMITED BY SIZE INTO PG-DESCRIPTION
           MOVE 2 TO PG-LEG-CNT
           MOVE "ROUNDING" TO PG-G-LEG-CODE(1)
           IF DH-FEE-ACCOUNT NOT = SPACES
               MOVE DH-FEE-ACCOUNT TO PG-G-ACCOUNT(1)
           END-IF
           MOVE DH-BANK-FEE TO PG-G-DEBIT(1)
           MOVE "Unpaid item charge" TO PG-G-NARRATIVE(1)
           MOVE "BANK" TO PG-G-LEG-CODE(2)
           MOVE DH-BANK-FEE TO PG-G-CREDIT(2)
           MOVE WS-R-BANK TO PG-G-BANK-ACCT(2)
           CALL "ARPOSTGL" USING WS-POSTGL-CA
           IF CA-RETURN-CODE OF WS-POSTGL-CA > 4
               MOVE 4 TO AR-RC
               MOVE "DIS0010" TO AR-REASON
               MOVE CA-MESSAGE OF WS-POSTGL-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       1500-EXIT.
           EXIT.

       1600-REASSESS-EXPOSURE SECTION.
       1600-START.
           INITIALIZE WS-CRED-CA
           MOVE "EXPOSURE" TO CA-FUNCTION OF WS-CRED-CA
           MOVE AR-CO TO CA-COMPANY OF WS-CRED-CA
           MOVE AR-USERID TO CA-USER OF WS-CRED-CA
           MOVE DH-POSTING-DATE TO CA-BUSINESS-DATE OF WS-CRED-CA
           MOVE DH-CUSTOMER TO CK-CUSTOMER
           MOVE DH-POSTING-DATE TO CK-EVAL-DATE
           CALL "CREDCHK" USING WS-CRED-CA
           MOVE CK-EXPOSURE TO DH-EXPOSURE-AFTER.
       1600-EXIT.
           EXIT.

       1700-RAISE-EXCEPTION SECTION.
       1700-START.
           INITIALIZE WS-EXCP-CA
           MOVE "RAISE" TO CA-FUNCTION OF WS-EXCP-CA
           MOVE AR-CO TO CA-COMPANY OF WS-EXCP-CA
           MOVE AR-USERID TO CA-USER OF WS-EXCP-CA
           MOVE DH-POSTING-DATE TO CA-BUSINESS-DATE OF WS-EXCP-CA
           MOVE "AR" TO EX-MODULE
           MOVE "WARNING" TO EX-SEVERITY
           MOVE "DISHONOURED-RECEIPT" TO EX-CATEGORY
           MOVE SPACES TO EX-MESSAGE-TEXT
           MOVE DH-RECEIPT-GBP TO AR-MONEY-UK
           STRING "Receipt " FUNCTION TRIM(DH-RECEIPT-NO)
                  " of GBP " FUNCTION TRIM(AR-MONEY-UK)
                  " was returned unpaid ("
                  FUNCTION TRIM(DH-SOURCE) ")"
               DELIMITED BY SIZE INTO EX-MESSAGE-TEXT
           MOVE "RECEIPT" TO EX-DOCUMENT-TYPE
           MOVE DH-RECEIPT-NO TO EX-DOCUMENT-REF
           MOVE "CREDMGR" TO EX-OWNER-ROLE
           MOVE 1 TO EX-PRIORITY
           MOVE DH-RECEIPT-GBP TO EX-AMOUNT-GBP
           CALL "EXCPRAIS" USING WS-EXCP-CA
           MOVE EX-EXCEPTION-ID TO DH-EXCEPTION-ID.
       1700-EXIT.
           EXIT.

      *>----------------------------------------------------------------
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
           MOVE "DIS0098" TO AR-REASON
           MOVE SPACES TO AR-MSG
           STRING "DISHONOR does not support function ["
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

       9500-ROLLBACK SECTION.
       9500-START.
           IF WS-TRAN-OPEN = "Y"
               MOVE "ROLB" TO SQLIO-FUNC
               MOVE 0 TO SQLIO-COL-CNT
               CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
               MOVE "N" TO WS-TRAN-OPEN
           END-IF.
       9500-EXIT.
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
