      *>================================================================
      *> PROGRAM-ID : APALLOC
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : ALLOCATION OF CASH, CREDITS, ADVANCES AND VALID
      *>              SETTLEMENT DISCOUNTS TO SUPPLIER OPEN ITEMS.
      *>              NOTHING IS EVER APPLIED BEYOND A RESIDUAL, SO
      *>              THE SAME INVOICE RESIDUAL, ADVANCE OR CREDIT
      *>              CANNOT BE CONSUMED TWICE (AP-014).
      *>              A FOREIGN CURRENCY SETTLEMENT KEEPS THE ORIGINAL
      *>              AMOUNT AND THE CARRYING VALUE AND RECORDS THE
      *>              DIFFERENCE AS A REALISED EXCHANGE GAIN OR LOSS -
      *>              NEVER AS A PURCHASE PRICE VARIANCE (AP-018).
      *>              REVERSING A SETTLEMENT REOPENS ONLY THE AFFECTED
      *>              AMOUNTS; A BANK RECONCILED PAYMENT IS CORRECTED
      *>              BY A TRACEABLE REVERSAL, NEVER ERASED (AP-016).
      *> CALLED BY  : .NET ALLOCATION FORM, APPAYRUN CALLERS, APCLOSE
      *> INTERFACE  : CALL "APALLOC" USING WS-APALLOC-CA
      *>              COPYBOOK APALLOCCA
      *> CALLS      : SQLIO, APSVC, APPOST
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-08-14 RJH  ORIGINAL PURCHASE LEDGER CASH ALLOCATION
      *> 1994-03-22 DJM  CR0455 FOREIGN SETTLEMENT DIFFERENCES
      *> 2026-10-05 STERP REALISED FX KEPT OUT OF PRICE VARIANCE
      *>================================================================
      *> REQ: AP-014, AP-016, AP-017, AP-018, D-22
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APALLOC.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 14/08/1991.
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
       01  WS-ITEM-ROW.
           05  WS-T-ID                 PIC X(24).
           05  WS-T-SUPPLIER           PIC X(20).
           05  WS-T-DOCUMENT           PIC X(30).
           05  WS-T-TYPE               PIC X(12).
           05  WS-T-CURRENCY           PIC X(3).
           05  WS-T-RATE               PIC X(24).
           05  WS-T-RESIDUAL           PIC X(24).
           05  WS-T-RESIDGBP           PIC X(24).
           05  WS-T-RESERVED           PIC X(24).
           05  WS-T-STATUS             PIC X(12).
       01  WS-ONE-COL.
           05  WS-C-VALUE              PIC X(40).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-NUM                  PIC S9(15)V9(6) VALUE 0.
           05  WS-APPLY                PIC S9(13)V99 VALUE 0.
           05  WS-CARRY-GBP            PIC S9(13)V99 VALUE 0.
           05  WS-SETTLE-GBP           PIC S9(13)V99 VALUE 0.
           05  WS-AVAILABLE            PIC S9(13)V99 VALUE 0.
           05  WS-MONEY-ED             PIC -(11)9.99.
       LINKAGE SECTION.
       COPY APALLOCCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "ALLOCATE" PERFORM 2000-ALLOCATE THRU 2000-EXIT
               WHEN "UNALLOC"  PERFORM 5000-UNALLOC THRU 5000-EXIT
               WHEN "RESIDUAL" PERFORM 6000-RESIDUAL THRU 6000-EXIT
               WHEN "FXREAL"   PERFORM 3000-FX THRU 3000-EXIT
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
           MOVE "APALLOC" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "AP-014" TO CA-REQ-REF
           MOVE SPACES TO AL-NOTE
           MOVE 0 TO AL-APPLIED-TOTAL AL-APPLIED-GBP
           MOVE 0 TO AL-DISCOUNT-GBP AL-DIFFERENCE-GBP
           MOVE 0 TO AL-REALISED-FX-GBP
           MOVE "N" TO AL-POSTED
           MOVE FUNCTION UPPER-CASE(CA-COMPANY) TO CA-COMPANY
           IF AL-EFFECTIVE-DATE = 0
               MOVE CA-BUSINESS-DATE TO AL-EFFECTIVE-DATE
           END-IF
           IF AL-SOURCE-RATE = 0
               MOVE 1 TO AL-SOURCE-RATE
           END-IF
           IF AL-SETTLE-RATE = 0
               MOVE AL-SOURCE-RATE TO AL-SETTLE-RATE
           END-IF
           IF AL-DIFF-TOLERANCE = 0
               INITIALIZE WS-APSVC
               MOVE "PARMNUM" TO SVC-FUNC
               PERFORM 8900-SVC-HEADER THRU 8900-EXIT
               MOVE "AP.DIFFERENCE.AUTO.GBP" TO SVC-TEXT-1
               MOVE 1 TO SVC-NUM-1
               CALL "APSVC" USING WS-APSVC
               MOVE SVC-NUM-OUT TO AL-DIFF-TOLERANCE
           END-IF
           IF CA-COMPANY = SPACES
               MOVE "APZ0001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2000-ALLOCATE SECTION.
       2000-START.
           IF AL-TARGET-CNT = 0
               MOVE "APZ0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "At least one target open item is required"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF AL-TARGET-CNT > 30
               MOVE 30 TO AL-TARGET-CNT
           END-IF
           PERFORM 2100-READ-SOURCE THRU 2100-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           MOVE AL-SOURCE-RESIDUAL TO WS-AVAILABLE
           IF WS-AVAILABLE < 0
               COMPUTE WS-AVAILABLE = 0 - WS-AVAILABLE
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > AL-TARGET-CNT OR WS-FATAL
               PERFORM 2200-ONE-TARGET THRU 2200-EXIT
           END-PERFORM
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           COMPUTE AL-UNAPPLIED = WS-AVAILABLE
           PERFORM 3000-FX THRU 3000-EXIT
           IF AL-REALISED-FX-GBP NOT = 0
               PERFORM 4000-POST-FX THRU 4000-EXIT
           END-IF
           MOVE AL-APPLIED-GBP TO WS-MONEY-ED
           MOVE SPACES TO CA-MESSAGE
           IF AL-REALISED-FX-GBP NOT = 0
               MOVE AL-REALISED-FX-GBP TO WS-MONEY-ED
               MOVE 4 TO CA-RETURN-CODE
               MOVE "APZ0010" TO CA-REASON-CODE
               STRING "Allocated with a realised exchange "
                      FUNCTION TRIM(AL-FX-POSITION) " of GBP "
                      FUNCTION TRIM(WS-MONEY-ED)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           ELSE
               STRING "Allocated GBP " FUNCTION TRIM(WS-MONEY-ED)
                      " across " DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       2000-EXIT.
           EXIT.

       2100-READ-SOURCE SECTION.
       2100-START.
           IF AL-SOURCE-ITEM-ID = 0 AND AL-SOURCE-NUMBER = SPACES
               MOVE AL-SOURCE-AMOUNT TO AL-SOURCE-RESIDUAL
               GO TO 2100-EXIT
           END-IF
           MOVE "SOURCE" TO WS-ONE-COL
           PERFORM 7000-READ-ITEM THRU 7000-EXIT
           IF WS-FATAL
               GO TO 2100-EXIT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-T-ID)
           MOVE WS-NUM TO AL-SOURCE-ITEM-ID
           MOVE WS-T-SUPPLIER TO AL-SUPPLIER
           MOVE WS-T-CURRENCY TO AL-SOURCE-CCY
           COMPUTE AL-SOURCE-RESIDUAL =
               FUNCTION NUMVAL(WS-T-RESIDUAL)
           IF AL-SOURCE-AMOUNT = 0
               MOVE AL-SOURCE-RESIDUAL TO AL-SOURCE-AMOUNT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-T-RATE)
           IF WS-NUM NOT = 0
               MOVE WS-NUM TO AL-SOURCE-RATE
           END-IF.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> NOTHING IS EVER APPLIED BEYOND THE TARGET RESIDUAL OR BEYOND
      *> WHAT THE SOURCE STILL HAS LEFT.  A SMALL DIFFERENCE INSIDE THE
      *> D-22 TOLERANCE IS RECORDED AS A SETTLEMENT DIFFERENCE WITH A
      *> REASON; A LARGER ONE LEAVES THE RESIDUAL OPEN.
      *>----------------------------------------------------------------
       2200-ONE-TARGET SECTION.
       2200-START.
           PERFORM 7100-READ-TARGET THRU 7100-EXIT
           IF WS-FATAL
               GO TO 2200-EXIT
           END-IF
           COMPUTE AL-T-RESIDUAL(WS-I) =
               FUNCTION NUMVAL(WS-T-RESIDUAL)
           MOVE AL-T-APPLY(WS-I) TO WS-APPLY
           IF WS-APPLY = 0
               MOVE AL-T-RESIDUAL(WS-I) TO WS-APPLY
           END-IF
           IF WS-APPLY > AL-T-RESIDUAL(WS-I)
               MOVE AL-T-RESIDUAL(WS-I) TO WS-APPLY
               MOVE "Capped at the remaining residual"
                   TO AL-T-MESSAGE(WS-I)
           END-IF
           IF WS-APPLY > WS-AVAILABLE
               MOVE WS-AVAILABLE TO WS-APPLY
               MOVE "Capped at the remaining source amount"
                   TO AL-T-MESSAGE(WS-I)
           END-IF
           IF WS-APPLY <= 0
               MOVE "PARTIAL" TO AL-T-STATUS(WS-I)
               MOVE "Nothing left to apply" TO AL-T-MESSAGE(WS-I)
               MOVE AL-T-RESIDUAL(WS-I) TO AL-T-NEW-RESID(WS-I)
               GO TO 2200-EXIT
           END-IF
           MOVE WS-APPLY TO AL-T-APPLY(WS-I)
           MOVE WS-APPLY TO AL-T-APPLY-GBP(WS-I)
           COMPUTE AL-T-NEW-RESID(WS-I) =
               AL-T-RESIDUAL(WS-I) - WS-APPLY
                   - AL-T-DISCOUNT(WS-I)
           IF FUNCTION ABS(AL-T-NEW-RESID(WS-I))
              <= AL-DIFF-TOLERANCE
             AND AL-T-NEW-RESID(WS-I) NOT = 0
               MOVE AL-T-NEW-RESID(WS-I) TO AL-T-DIFF(WS-I)
               ADD AL-T-NEW-RESID(WS-I) TO AL-DIFFERENCE-GBP
               MOVE 0 TO AL-T-NEW-RESID(WS-I)
           END-IF
           IF AL-T-NEW-RESID(WS-I) <= 0
               MOVE "SETTLED" TO AL-T-STATUS(WS-I)
           ELSE
               MOVE "PARTIAL" TO AL-T-STATUS(WS-I)
           END-IF
           SUBTRACT WS-APPLY FROM WS-AVAILABLE
           ADD WS-APPLY TO AL-APPLIED-TOTAL
           ADD WS-APPLY TO AL-APPLIED-GBP
           ADD AL-T-DISCOUNT(WS-I) TO AL-DISCOUNT-GBP
           PERFORM 2300-WRITE-ALLOCATION THRU 2300-EXIT
           PERFORM 2400-UPDATE-TARGET THRU 2400-EXIT.
       2200-EXIT.
           EXIT.

       2300-WRITE-ALLOCATION SECTION.
       2300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; "
                  "INSERT ap.Allocation (CompanyCode, SupplierCode, "
                  "SourceType, SourceOpenItemId, TargetOpenItemId, "
                  "PaymentInstructionId, RemittanceRef, "
                  "EffectiveDate, CurrencyCode, AppliedAmount, "
                  "AppliedAmountGBP, DiscountAmountGBP, "
                  "DifferenceGBP, DifferenceReason, RealisedFxGBP, "
                  "CreatedBy) VALUES (?, ?, ?, ?, ?, NULLIF(?,0), "
                  "NULLIF(?,''), CAST(? AS date), ?, ?, ?, ?, ?, "
                  "NULLIF(?,''), ?, ?); "
                  "SELECT CAST(SCOPE_IDENTITY() AS varchar(24))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE AL-SUPPLIER TO SQLIO-PARM-VAL(2)
           IF AL-SOURCE-TYPE = SPACES
               MOVE "PAYMENT" TO AL-SOURCE-TYPE
           END-IF
           MOVE AL-SOURCE-TYPE TO SQLIO-PARM-VAL(3)
           MOVE AL-SOURCE-ITEM-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE AL-T-ITEM-ID(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE AL-INSTRUCTION-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE AL-REMITTANCE-REF TO SQLIO-PARM-VAL(7)
           MOVE AL-EFFECTIVE-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE AL-T-CURRENCY(WS-I) TO SQLIO-PARM-VAL(9)
           IF SQLIO-PARM-VAL(9) = SPACES
               MOVE "GBP" TO SQLIO-PARM-VAL(9)
           END-IF
           MOVE AL-T-APPLY(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE AL-T-APPLY-GBP(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE AL-T-DISCOUNT(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
           MOVE AL-T-DIFF(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
           MOVE AL-REASON(1:200) TO SQLIO-PARM-VAL(14)
           MOVE AL-T-FX-GBP(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(15)
           MOVE CA-USER TO SQLIO-PARM-VAL(16)
           MOVE 16 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 16
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2300-EXIT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-C-VALUE)
           MOVE WS-NUM TO AL-ALLOCATION-ID.
       2300-EXIT.
           EXIT.

       2400-UPDATE-TARGET SECTION.
       2400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.OpenItem SET ResidualAmount = ?, "
                  "ResidualAmountGBP = ?, ReservedAmountGBP = CASE "
                  "WHEN ReservedAmountGBP - ? < 0 THEN 0 ELSE "
                  "ReservedAmountGBP - ? END, SettlementStatus = ?, "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE OpenItemId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE AL-T-NEW-RESID(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE AL-T-APPLY(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE AL-T-STATUS(WS-I) TO SQLIO-PARM-VAL(5)
           MOVE CA-USER TO SQLIO-PARM-VAL(6)
           MOVE AL-T-ITEM-ID(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE 7 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 7
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       2400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AP-018.  THE LIABILITY WAS CARRIED AT THE INVOICE RATE AND
      *> SETTLED AT THE PAYMENT RATE.  THE DIFFERENCE BETWEEN THE TWO
      *> STERLING AMOUNTS IS A REALISED EXCHANGE GAIN OR LOSS, NOT A
      *> PURCHASE PRICE VARIANCE.
      *>----------------------------------------------------------------
       3000-FX SECTION.
       3000-START.
           MOVE 0 TO AL-REALISED-FX-GBP
           MOVE SPACES TO AL-FX-POSITION
           IF AL-SOURCE-CCY = SPACES OR AL-SOURCE-CCY = "GBP"
               GO TO 3000-EXIT
           END-IF
           IF AL-SOURCE-RATE = AL-SETTLE-RATE
               GO TO 3000-EXIT
           END-IF
           INITIALIZE WS-APSVC
           MOVE "FXREAL" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE AL-SOURCE-CCY TO SVC-TEXT-1
           MOVE AL-EFFECTIVE-DATE TO SVC-DATE-1
           IF AL-APPLIED-TOTAL NOT = 0
               MOVE AL-APPLIED-TOTAL TO SVC-NUM-1
           ELSE
               MOVE AL-SOURCE-AMOUNT TO SVC-NUM-1
           END-IF
           MOVE AL-SOURCE-RATE TO SVC-NUM-2
           MOVE AL-SETTLE-RATE TO SVC-NUM-3
           CALL "APSVC" USING WS-APSVC
           MOVE SVC-NUM-OUT2 TO WS-CARRY-GBP
           MOVE SVC-NUM-3 TO WS-SETTLE-GBP
           IF WS-CARRY-GBP = 0
               COMPUTE WS-CARRY-GBP ROUNDED =
                   SVC-NUM-1 / AL-SOURCE-RATE
           END-IF
           IF WS-SETTLE-GBP = 0
               COMPUTE WS-SETTLE-GBP ROUNDED =
                   SVC-NUM-1 / AL-SETTLE-RATE
           END-IF
           COMPUTE AL-REALISED-FX-GBP =
               WS-SETTLE-GBP - WS-CARRY-GBP
           IF AL-REALISED-FX-GBP > 0
               MOVE "LOSS" TO AL-FX-POSITION
           ELSE
               MOVE "GAIN" TO AL-FX-POSITION
           END-IF
           MOVE AL-REALISED-FX-GBP TO AL-T-FX-GBP(1)
           MOVE WS-CARRY-GBP TO AL-APPLIED-GBP.
       3000-EXIT.
           EXIT.

       4000-POST-FX SECTION.
       4000-START.
           IF CA-CORRELATION-ID = SPACES
               GO TO 4000-EXIT
           END-IF
           INITIALIZE WS-APPOST
           MOVE "POST" TO PJ-FUNC
           MOVE CA-COMPANY TO PJ-COMPANY
           MOVE CA-SITE TO PJ-SITE
           MOVE CA-USER TO PJ-USER
           MOVE CA-BUSINESS-DATE TO PJ-BUSINESS-DATE
           MOVE SPACES TO PJ-CORRELATION
           STRING FUNCTION TRIM(CA-CORRELATION-ID) "-FX"
               DELIMITED BY SIZE INTO PJ-CORRELATION
           IF AL-FX-POSITION = "LOSS"
               MOVE "AP.FXLOSS" TO PJ-EVENT-CODE
           ELSE
               MOVE "AP.FXGAIN" TO PJ-EVENT-CODE
           END-IF
           MOVE "AP" TO PJ-JOURNAL-TYPE
           MOVE AL-SOURCE-NUMBER TO PJ-SOURCE-REF
           MOVE SPACES TO PJ-DESCRIPTION
           STRING "Realised exchange difference on settlement of "
                  FUNCTION TRIM(AL-SOURCE-NUMBER)
               DELIMITED BY SIZE INTO PJ-DESCRIPTION
           MOVE AL-EFFECTIVE-DATE TO PJ-POSTING-DATE
           MOVE AL-EFFECTIVE-DATE TO PJ-DOCUMENT-DATE
           MOVE "GBP" TO PJ-CURRENCY
           MOVE 1 TO PJ-FX-RATE
           MOVE "JNL" TO PJ-SERIES-CODE
           MOVE "AP" TO PJ-SUBLEDGER-TYPE
           MOVE AL-SUPPLIER TO PJ-SUBLEDGER-REF
           MOVE 2 TO PJ-LINE-CNT
           IF AL-FX-POSITION = "LOSS"
               MOVE "7100" TO PJ-L-ACCOUNT(1)
               MOVE AL-REALISED-FX-GBP TO PJ-L-DR(1)
               MOVE "ADMIN" TO PJ-L-COST-CTR(1)
               MOVE "Realised exchange loss" TO PJ-L-NARRATIVE(1)
               MOVE "2000" TO PJ-L-ACCOUNT(2)
               MOVE AL-REALISED-FX-GBP TO PJ-L-CR(2)
               MOVE "Trade creditors control" TO PJ-L-NARRATIVE(2)
           ELSE
               MOVE "2000" TO PJ-L-ACCOUNT(1)
               COMPUTE PJ-L-DR(1) = 0 - AL-REALISED-FX-GBP
               MOVE "Trade creditors control" TO PJ-L-NARRATIVE(1)
               MOVE "7100" TO PJ-L-ACCOUNT(2)
               COMPUTE PJ-L-CR(2) = 0 - AL-REALISED-FX-GBP
               MOVE "ADMIN" TO PJ-L-COST-CTR(2)
               MOVE "Realised exchange gain" TO PJ-L-NARRATIVE(2)
           END-IF
           CALL "APPOST" USING WS-APPOST
           IF PJ-RC < 8
               MOVE "Y" TO AL-POSTED
               MOVE PJ-JOURNAL-NUMBER TO AL-JOURNAL-NUMBER
           END-IF.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AP-016.  A REVERSAL REOPENS ONLY THE AFFECTED AMOUNTS.  THE
      *> ORIGINAL ALLOCATION IS MARKED REVERSED AND LINKED, NEVER
      *> DELETED, SO THE BANK CONSEQUENCES STAY TRACEABLE.
      *>----------------------------------------------------------------
       5000-UNALLOC SECTION.
       5000-START.
           IF AL-REVERSE-ID = 0
               MOVE "APZ0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The allocation to reverse must be identified"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           IF AL-REASON = SPACES OR AL-REASON = LOW-VALUES
               MOVE "APZ0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Reversing a settlement needs a recorded "
                      "reason" DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE oi SET ResidualAmount = oi.ResidualAmount +"
                  " CASE WHEN ? = 0 THEN a.AppliedAmount ELSE "
                  "CAST(? AS decimal(18,2)) END, ResidualAmountGBP = "
                  "oi.ResidualAmountGBP + CASE WHEN ? = 0 THEN "
                  "a.AppliedAmountGBP ELSE CAST(? AS decimal(18,2)) "
                  "END, SettlementStatus = 'PARTIAL', ModifiedBy = ?,"
                  " ModifiedAt = SYSDATETIME() FROM ap.OpenItem oi "
                  "JOIN ap.Allocation a ON a.TargetOpenItemId = "
                  "oi.OpenItemId WHERE a.AllocationId = ? "
                  "AND a.Status = 'ACTIVE'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE AL-REVERSE-AMOUNT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE CA-USER TO SQLIO-PARM-VAL(5)
           MOVE AL-REVERSE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE 6 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ap.Allocation (CompanyCode, SupplierCode, "
                  "SourceType, SourceOpenItemId, TargetOpenItemId, "
                  "EffectiveDate, CurrencyCode, AppliedAmount, "
                  "AppliedAmountGBP, DifferenceReason, Status, "
                  "ReversalOfAllocationId, CreatedBy) "
                  "SELECT a.CompanyCode, a.SupplierCode, "
                  "a.SourceType, a.SourceOpenItemId, "
                  "a.TargetOpenItemId, CAST(? AS date), "
                  "a.CurrencyCode, -CASE WHEN ? = 0 THEN "
                  "a.AppliedAmount ELSE CAST(? AS decimal(18,2)) END,"
                  " -CASE WHEN ? = 0 THEN a.AppliedAmountGBP ELSE "
                  "CAST(? AS decimal(18,2)) END, ?, 'REVERSED', "
                  "a.AllocationId, ? FROM ap.Allocation a "
                  "WHERE a.AllocationId = ?; "
                  "UPDATE ap.Allocation SET Status = 'REVERSED' "
                  "WHERE AllocationId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE AL-EFFECTIVE-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE AL-REVERSE-AMOUNT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE AL-REASON(1:200) TO SQLIO-PARM-VAL(6)
           MOVE CA-USER TO SQLIO-PARM-VAL(7)
           MOVE AL-REVERSE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE 9 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 9
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
           MOVE 4 TO CA-RETURN-CODE
           MOVE "APZ0011" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "The settlement was reversed under a linked "
                  "correction; the original allocation is retained"
               DELIMITED BY SIZE INTO CA-MESSAGE
           ADD 1 TO CA-MSG-COUNT
           INITIALIZE WS-APSVC
           MOVE "AUDIT" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "ap.Allocation" TO SVC-AUD-ENTITY
           MOVE AL-REVERSE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SVC-AUD-KEY
           MOVE "REVERSE" TO SVC-AUD-ACTION
           MOVE "Status" TO SVC-AUD-FIELD
           MOVE "ACTIVE" TO SVC-AUD-OLD
           MOVE "REVERSED" TO SVC-AUD-NEW
           MOVE AL-REASON TO SVC-AUD-REASON
           MOVE "N" TO SVC-AUD-SENS
           CALL "APSVC" USING WS-APSVC.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       6000-RESIDUAL SECTION.
       6000-START.
           MOVE "SOURCE" TO WS-ONE-COL
           PERFORM 7000-READ-ITEM THRU 7000-EXIT
           IF WS-FATAL
               GO TO 6000-EXIT
           END-IF
           COMPUTE AL-SOURCE-RESIDUAL =
               FUNCTION NUMVAL(WS-T-RESIDUAL)
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-T-RESERVED)
           COMPUTE AL-UNAPPLIED = AL-SOURCE-RESIDUAL - WS-NUM
           MOVE WS-T-SUPPLIER TO AL-SUPPLIER
           MOVE WS-T-CURRENCY TO AL-SOURCE-CCY
           MOVE AL-UNAPPLIED TO WS-MONEY-ED
           MOVE SPACES TO CA-MESSAGE
           STRING "Residual available after reservation: "
                  FUNCTION TRIM(WS-MONEY-ED)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       6000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       7000-READ-ITEM SECTION.
       7000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CAST(OpenItemId AS varchar(24)), "
                  "SupplierCode, DocumentNumber, ItemType, "
                  "CurrencyCode, CAST(FxRate AS varchar(24)), "
                  "CAST(ResidualAmount AS varchar(24)), "
                  "CAST(ResidualAmountGBP AS varchar(24)), "
                  "CAST(ReservedAmountGBP AS varchar(24)), "
                  "SettlementStatus FROM ap.OpenItem "
                  "WHERE CompanyCode = ? AND ((? <> 0 AND "
                  "OpenItemId = ?) OR (? <> '' AND "
                  "DocumentNumber = ?))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE AL-SOURCE-ITEM-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE AL-SOURCE-NUMBER TO SQLIO-PARM-VAL(4)
           MOVE AL-SOURCE-NUMBER TO SQLIO-PARM-VAL(5)
           PERFORM 7200-FETCH-ITEM THRU 7200-EXIT.
       7000-EXIT.
           EXIT.

       7100-READ-TARGET SECTION.
       7100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CAST(OpenItemId AS varchar(24)), "
                  "SupplierCode, DocumentNumber, ItemType, "
                  "CurrencyCode, CAST(FxRate AS varchar(24)), "
                  "CAST(ResidualAmount AS varchar(24)), "
                  "CAST(ResidualAmountGBP AS varchar(24)), "
                  "CAST(ReservedAmountGBP AS varchar(24)), "
                  "SettlementStatus FROM ap.OpenItem "
                  "WHERE CompanyCode = ? AND ((? <> 0 AND "
                  "OpenItemId = ?) OR (? <> '' AND "
                  "DocumentNumber = ?))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE AL-T-ITEM-ID(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE AL-T-DOCUMENT(WS-I) TO SQLIO-PARM-VAL(4)
           MOVE AL-T-DOCUMENT(WS-I) TO SQLIO-PARM-VAL(5)
           PERFORM 7200-FETCH-ITEM THRU 7200-EXIT
           IF NOT WS-FATAL
               COMPUTE WS-NUM = FUNCTION NUMVAL(WS-T-ID)
               MOVE WS-NUM TO AL-T-ITEM-ID(WS-I)
               MOVE WS-T-DOCUMENT TO AL-T-DOCUMENT(WS-I)
               MOVE WS-T-CURRENCY TO AL-T-CURRENCY(WS-I)
           END-IF.
       7100-EXIT.
           EXIT.

       7200-FETCH-ITEM SECTION.
       7200-START.
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 10 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 30 TO SQLIO-COL-WIDTH(3)
           MOVE 12 TO SQLIO-COL-WIDTH(4)
           MOVE 3 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 24 TO SQLIO-COL-WIDTH(8)
           MOVE 24 TO SQLIO-COL-WIDTH(9)
           MOVE 12 TO SQLIO-COL-WIDTH(10)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ITEM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 7200-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE
               MOVE "APZ0005" TO CA-REASON-CODE
               MOVE "That supplier open item was not found"
                   TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
           END-IF.
       7200-EXIT.
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
           MOVE "APZ0099" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "APALLOC does not support function "
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
               MOVE "SQL failure in APALLOC" TO CA-MESSAGE
           END-IF
           ADD 1 TO CA-MSG-COUNT
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       9300-EXIT.
           EXIT.
