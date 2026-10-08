      *>================================================================
      *> PROGRAM-ID : RCPTALOC
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : CUSTOMER RECEIPTS AND THE ALLOCATION OF RECEIPTS
      *>              AND CREDITS TO OPEN ITEMS.  A RECEIPT IS THE
      *>              ONLY CASH EVENT; ALLOCATION MOVES RESIDUALS AND
      *>              HAS NO SECOND CASH EFFECT.  SETTLEMENT
      *>              DISCOUNTS, SMALL DIFFERENCES AND WRITE OFFS ARE
      *>              SEPARATELY CLASSIFIED AND AUTHORISED (D-22).
      *>              UNALLOCATION PRESERVES THE ORIGINAL HISTORY AND
      *>              DOES NOT REVERSE THE RECEIPT.
      *> CALLED BY  : RECEIPT ENTRY AND ALLOCATION SCREEN, DDCOLL,
      *>              DISHONOR, AR DEMO BATCH, .NET CLIENT
      *> INTERFACE  : CALL "RCPTALOC" USING WS-RCPTALOC-CA
      *>              COPYBOOK RCPTALOCCA
      *> CALLS      : SQLIO, PERMCHK, PERIODCK, NUMGEN, IDEMPOT,
      *>              PARMGET, FXCONV, ARPOSTGL, EXCPRAIS
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-06-11 RJH  ORIGINAL MVS CASH MATCHING SUITE
      *> 1994-03-07 DJM  CR0312 PART ALLOCATION WITH RESIDUAL PREVIEW
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS
      *> 2004-10-01 DJM  CR1744 SMALL DIFFERENCE WRITE OFF LIMIT
      *> 2026-10-05 STERP CROSS CURRENCY INSTRUCTION AND REALISED FX
      *> 2026-10-07 STERP CR2611 RECEIPT NAMES THE CREDIT IT CREATES IN
      *>                 FULL (ITEM ID, DOCUMENT, TYPE, AVAILABLE) SO
      *>                 THE NEXT ALLOCATE FINDS IT
      *> 2026-10-07 STERP CR2612 AN ALLOCATION MUST NAME ITS SOURCE;
      *>                 IT NO LONGER ADOPTS THE FIRST OPEN ITEM HELD
      *>================================================================
      *> REQ: AR-009, AR-010, AR-011, AR-012, AR-019
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RCPTALOC.
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
       COPY NUMGENCA REPLACING ==DFHCOMMAREA== BY ==WS-NUM-CA==.
       COPY IDEMPOTCA REPLACING ==DFHCOMMAREA== BY ==WS-IDEM-CA==.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARM-CA==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXCP-CA==.
       COPY ARPOSTGLCA REPLACING ==DFHCOMMAREA== BY ==WS-POSTGL-CA==.
       01  WS-SRC.
           05  WS-S-ID                 PIC X(24).
           05  WS-S-CUSTOMER           PIC X(20).
           05  WS-S-CURRENCY           PIC X(3).
           05  WS-S-RATE               PIC X(24).
           05  WS-S-RESIDUAL           PIC X(24).
           05  WS-S-RESID-GBP          PIC X(24).
           05  WS-S-TYPE               PIC X(12).
           05  WS-S-DOCNO              PIC X(30).
           05  WS-S-CONFIRM            PIC X(12).
           05  WS-S-CONTROL            PIC X(12).
       01  WS-TGT.
           05  WS-T-ID                 PIC X(24).
           05  WS-T-CUSTOMER           PIC X(20).
           05  WS-T-CURRENCY           PIC X(3).
           05  WS-T-RATE               PIC X(24).
           05  WS-T-RESIDUAL           PIC X(24).
           05  WS-T-RESID-GBP          PIC X(24).
           05  WS-T-TYPE               PIC X(12).
           05  WS-T-DOCNO              PIC X(30).
           05  WS-T-DISC-DATE          PIC X(10).
           05  WS-T-DISC-AMT           PIC X(24).
           05  WS-T-DUE-DATE           PIC X(10).
           05  WS-T-DISPUTED           PIC X(4).
       01  WS-ID-ROW.
           05  WS-NEW-ID               PIC X(24).
           05  WS-ID2                  PIC X(24).
       01  WS-SW.
           05  WS-TRAN-OPEN            PIC X VALUE "N".
           05  WS-LEG                  PIC S9(4) VALUE 0.
      *>   COM-015.  fin.usp_PostJournal keys core.IdempotencyKey on
      *>   the correlation id by itself and this program posts twice:
      *>   the cash entry and then the allocation adjustments.  The
      *>   second posting carries a derived id built on a 30 character
      *>   stem - CR2741: the suffix used to be appended to the whole
      *>   36 character caller id and was truncated straight back off
      *>   again, so discounts, write offs and realised FX were never
      *>   posted to the nominal at all.
           05  WS-CORR-STEM            PIC X(30) VALUE SPACES.
           05  WS-ALLOC-CORR           PIC X(36) VALUE SPACES.
           05  WS-PREVIEW              PIC X VALUE "N".
           05  WS-SRC-LEFT             PIC S9(11)V99 VALUE 0.
           05  WS-SRC-RATE             PIC S9(9)V9(8) VALUE 0.
           05  WS-TGT-RATE             PIC S9(9)V9(8) VALUE 0.
           05  WS-APPLY-GBP            PIC S9(11)V99 VALUE 0.
           05  WS-DISC-GBP             PIC S9(11)V99 VALUE 0.
           05  WS-WO-GBP               PIC S9(11)V99 VALUE 0.
       LINKAGE SECTION.
       COPY RCPTALOCCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF AR-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE AR-FUNC
               WHEN "RECEIPT"  PERFORM 1000-RECORD-RECEIPT
               WHEN "CONFIRM"  PERFORM 1800-CONFIRM-RECEIPT
               WHEN "PREVIEW"  MOVE "Y" TO WS-PREVIEW
                               PERFORM 2000-ALLOCATE
               WHEN "ALLOCATE" PERFORM 2000-ALLOCATE
               WHEN "UNALLOC"  PERFORM 3000-UNALLOCATE
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
           INITIALIZE WS-SRC
           INITIALIZE WS-TGT
           INITIALIZE WS-SW
           MOVE "N" TO WS-TRAN-OPEN
           MOVE "N" TO WS-PREVIEW
           MOVE CA-FUNCTION OF DFHCOMMAREA TO AR-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO AR-CO
           MOVE CA-SITE OF DFHCOMMAREA TO AR-SITE
           MOVE CA-USER OF DFHCOMMAREA TO AR-USERID
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO AR-BUSDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO AR-CORREL
           MOVE "RCPTALOC" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO AR-RC AR-MSG-CNT
           MOVE SPACES TO AR-REASON AR-MSG
           MOVE "AR-010" TO AR-REQ-REF
           MOVE 0 TO RA-TOT-APPLIED RA-TOT-DISCOUNT
           MOVE 0 TO RA-TOT-WRITEOFF RA-TOT-FX RA-RESIDUAL
           MOVE 0 TO RA-REVERSED-CNT
           IF RA-RECEIPT-DATE = 0
               MOVE AR-BUSDATE TO RA-RECEIPT-DATE
           END-IF
           IF RA-POSTING-DATE = 0
               MOVE RA-RECEIPT-DATE TO RA-POSTING-DATE
           END-IF
           IF RA-EFFECTIVE-DATE = 0
               MOVE RA-POSTING-DATE TO RA-EFFECTIVE-DATE
           END-IF
           IF RA-CURRENCY = SPACES
               MOVE "GBP" TO RA-CURRENCY
           END-IF
           IF RA-FX-RATE = 0
               MOVE 1 TO RA-FX-RATE
           END-IF
           IF RA-CONFIRM-STATE = SPACES
               MOVE "CONFIRMED" TO RA-CONFIRM-STATE
           END-IF
           IF RA-SOURCE-TYPE = SPACES
               MOVE "RECEIPT" TO RA-SOURCE-TYPE
           END-IF
           PERFORM 0150-READ-LIMITS.
       0100-EXIT.
           EXIT.

      *>   D-22 small difference limit and the discount grace
       0150-READ-LIMITS SECTION.
       0150-START.
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE AR-USERID TO CA-USER OF WS-PARM-CA
           MOVE RA-POSTING-DATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "AR.WRITEOFF.AUTO.GBP" TO PM-PARAM-CODE
           MOVE "Y" TO PM-USE-DEFAULT
           MOVE "5.00" TO PM-DEFAULT-TEXT
           CALL "PARMGET" USING WS-PARM-CA
           MOVE PM-VALUE-NUM TO RA-WRITEOFF-LIMIT
           IF RA-WRITEOFF-LIMIT = 0
               MOVE 5 TO RA-WRITEOFF-LIMIT
           END-IF
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE AR-USERID TO CA-USER OF WS-PARM-CA
           MOVE RA-POSTING-DATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "AR.DISCOUNT.GRACE.DAYS" TO PM-PARAM-CODE
           MOVE "Y" TO PM-USE-DEFAULT
           MOVE "2" TO PM-DEFAULT-TEXT
           CALL "PARMGET" USING WS-PARM-CA
           MOVE PM-VALUE-NUM TO RA-DISC-GRACE-DAYS.
       0150-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF AR-CO = SPACES
               MOVE "RCP0001" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A company code is required" DELIMITED BY SIZE
                   INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AR-FUNC = "RECEIPT"
               IF RA-GROSS <= 0
                   MOVE "RCP0002" TO AR-REASON
                   MOVE SPACES TO AR-MSG
                   STRING "A receipt must be for a positive amount"
                       DELIMITED BY SIZE INTO AR-MSG
                   PERFORM 9000-REJECT
                   GO TO 0200-EXIT
               END-IF
               IF RA-BANK-ACCOUNT = SPACES
                   MOVE "RCP0003" TO AR-REASON
                   MOVE SPACES TO AR-MSG
                   STRING "A bank or cash account is required so the "
                          "money can be identified"
                       DELIMITED BY SIZE INTO AR-MSG
                   PERFORM 9000-REJECT
                   GO TO 0200-EXIT
               END-IF
               IF RA-METHOD = SPACES
                   MOVE "BACS" TO RA-METHOD
               END-IF
           END-IF
           IF AR-FUNC = "ALLOCATE" OR AR-FUNC = "PREVIEW"
               IF RA-ALLOC-CNT < 1 OR RA-ALLOC-CNT > 20
                   MOVE "RCP0004" TO AR-REASON
                   MOVE SPACES TO AR-MSG
                   STRING "An allocation instruction needs between "
                          "one and twenty target items"
                       DELIMITED BY SIZE INTO AR-MSG
                   PERFORM 9000-REJECT
               END-IF
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   AR-009 - record the receipt.  Confirmed funds post cash;
      *>   unconfirmed evidence is held as an expected receipt and is
      *>   not eligible to settle anything until it is confirmed.
       1000-RECORD-RECEIPT SECTION.
       1000-START.
           PERFORM 1050-PERMISSION
           IF AR-RC-BAD
               GO TO 1000-EXIT
           END-IF
           PERFORM 1100-PERIOD
           IF AR-RC-BAD
               GO TO 1000-EXIT
           END-IF
           PERFORM 1150-IDEMPOTENT-BEGIN
           IF AR-RC-BAD OR AR-RC-WARN
               GO TO 1000-EXIT
           END-IF
           PERFORM 1200-DRAW-NUMBER
           IF AR-RC-BAD
               GO TO 1000-EXIT
           END-IF
           IF RA-CURRENCY = "GBP"
               MOVE RA-GROSS TO RA-GROSS-GBP
               MOVE 1 TO RA-FX-RATE
           ELSE
               IF RA-FX-RATE <= 0
                   MOVE 8 TO AR-RC
                   MOVE "RCP0005" TO AR-REASON
                   MOVE SPACES TO AR-MSG
                   STRING "A foreign receipt needs an approved rate "
                          "before it can be valued in sterling"
                       DELIMITED BY SIZE INTO AR-MSG
                   GO TO 1000-EXIT
               END-IF
               COMPUTE RA-GROSS-GBP ROUNDED = RA-GROSS / RA-FX-RATE
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "Y" TO WS-TRAN-OPEN
           PERFORM 1300-WRITE-RECEIPT
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           IF RA-CONFIRMED
               PERFORM 1400-OPEN-ITEM
               IF AR-RC-BAD
                   PERFORM 9500-ROLLBACK
                   GO TO 1000-EXIT
               END-IF
               PERFORM 1500-POST-CASH
               IF AR-RC-BAD
                   PERFORM 9500-ROLLBACK
                   GO TO 1000-EXIT
               END-IF
           END-IF
           IF RA-UNIDENTIFIED = "Y"
               PERFORM 1600-UNIDENTIFIED-QUEUE
           END-IF
           MOVE "COMT" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "N" TO WS-TRAN-OPEN
           PERFORM 1950-IDEMPOTENT-END
           MOVE RA-GROSS TO AR-MONEY-UK
           MOVE SPACES TO AR-MSG
           STRING "Receipt " FUNCTION TRIM(RA-RECEIPT-NO) " "
                  FUNCTION TRIM(RA-CURRENCY) " "
                  FUNCTION TRIM(AR-MONEY-UK) " recorded ("
                  FUNCTION TRIM(RA-CONFIRM-STATE) ")"
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
           MOVE RA-POSTING-DATE TO CA-BUSINESS-DATE OF WS-PERM-CA
           IF AR-FUNC = "RECEIPT" OR AR-FUNC = "CONFIRM"
               MOVE "AR.RECEIPT.POST" TO PK-PERMISSION
           ELSE
               MOVE "AR.ALLOCATE.RUN" TO PK-PERMISSION
           END-IF
           CALL "PERMCHK" USING WS-PERM-CA
           IF CA-RETURN-CODE OF WS-PERM-CA > 4
               MOVE 6 TO AR-RC
               MOVE "RCP0006" TO AR-REASON
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
           MOVE RA-POSTING-DATE TO CA-BUSINESS-DATE OF WS-PERIOD-CA
           MOVE RA-POSTING-DATE TO PC-POSTING-DATE
           MOVE RA-RECEIPT-DATE TO PC-DOCUMENT-DATE
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

       1150-IDEMPOTENT-BEGIN SECTION.
       1150-START.
           IF AR-CORREL = SPACES
               MOVE 8 TO AR-RC
               MOVE "RCP0007" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A correlation id is required so a repeated "
                      "bank receipt does not increase cash twice"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 1150-EXIT
           END-IF
           INITIALIZE WS-IDEM-CA
           MOVE "BEGIN" TO CA-FUNCTION OF WS-IDEM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-IDEM-CA
           MOVE AR-USERID TO CA-USER OF WS-IDEM-CA
           MOVE AR-CORREL TO CA-CORRELATION-ID OF WS-IDEM-CA
           MOVE RA-POSTING-DATE TO CA-BUSINESS-DATE OF WS-IDEM-CA
           MOVE "ar.Receipt.Post" TO ID-OPERATION
           CALL "IDEMPOT" USING WS-IDEM-CA
           EVALUATE CA-RETURN-CODE OF WS-IDEM-CA
               WHEN 0      CONTINUE
               WHEN 4      MOVE ID-RESULT-REF TO RA-RECEIPT-NO
                           MOVE 4 TO AR-RC
                           MOVE "RCP0008" TO AR-REASON
                           MOVE CA-MESSAGE OF WS-IDEM-CA TO AR-MSG
                           ADD 1 TO AR-MSG-CNT
               WHEN OTHER  MOVE CA-RETURN-CODE OF WS-IDEM-CA TO AR-RC
                           MOVE CA-REASON-CODE OF WS-IDEM-CA
                               TO AR-REASON
                           MOVE CA-MESSAGE OF WS-IDEM-CA TO AR-MSG
                           ADD 1 TO AR-MSG-CNT
           END-EVALUATE.
       1150-EXIT.
           EXIT.

       1200-DRAW-NUMBER SECTION.
       1200-START.
           IF RA-RECEIPT-NO NOT = SPACES
               GO TO 1200-EXIT
           END-IF
           INITIALIZE WS-NUM-CA
           MOVE "NEXT" TO CA-FUNCTION OF WS-NUM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-NUM-CA
           MOVE AR-USERID TO CA-USER OF WS-NUM-CA
           MOVE AR-CORREL TO CA-CORRELATION-ID OF WS-NUM-CA
           MOVE RA-RECEIPT-DATE TO CA-BUSINESS-DATE OF WS-NUM-CA
           MOVE "ARRC" TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUM-CA
           IF CA-RETURN-CODE OF WS-NUM-CA > 4
               MOVE CA-RETURN-CODE OF WS-NUM-CA TO AR-RC
               MOVE CA-REASON-CODE OF WS-NUM-CA TO AR-REASON
               MOVE CA-MESSAGE OF WS-NUM-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 1200-EXIT
           END-IF
           MOVE NG-DOCUMENT-NUMBER TO RA-RECEIPT-NO.
       1200-EXIT.
           EXIT.

       1300-WRITE-RECEIPT SECTION.
       1300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT ar.Receipt (CompanyCode, "
                  "ReceiptNumber, CustomerCode, PayerCustomerCode, "
                  "PayerName, PaymentMethod, BankAccountCode, "
                  "ReceiptDate, ValueDate, PostingDate, CurrencyCode,"
                  " FxRate, GrossReceived, GrossReceivedGBP, BankFee,"
                  " AllocatedAmount, UnallocatedAmount, "
                  "BankReference, ExternalReference, "
                  "ConfirmationState, Status, CorrelationId, "
                  "CreatedBy) VALUES (?, ?, NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(?,''), ?, ?, ?, NULLIF(?,''),"
                  " ?, ?, ?, ?, ?, ?, 0, ?, NULLIF(?,''), "
                  "NULLIF(?,''), ?, 'POSTED', ?, ?); "
                  "SELECT CONVERT(varchar(24), SCOPE_IDENTITY()), ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 20 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE RA-RECEIPT-NO TO SQLIO-PARM-VAL(2)
           MOVE RA-CUSTOMER TO SQLIO-PARM-VAL(3)
           MOVE RA-PAYER TO SQLIO-PARM-VAL(4)
           MOVE RA-PAYER-NAME TO SQLIO-PARM-VAL(5)
           MOVE RA-METHOD TO SQLIO-PARM-VAL(6)
           MOVE RA-BANK-ACCOUNT TO SQLIO-PARM-VAL(7)
           MOVE RA-RECEIPT-DATE TO SQLIO-PARM-VAL(8)
           IF RA-VALUE-DATE = 0
               MOVE SPACES TO SQLIO-PARM-VAL(9)
           ELSE
               MOVE RA-VALUE-DATE TO SQLIO-PARM-VAL(9)
           END-IF
           MOVE RA-POSTING-DATE TO SQLIO-PARM-VAL(10)
           MOVE RA-CURRENCY TO SQLIO-PARM-VAL(11)
           MOVE RA-FX-RATE TO AR-RATE-ED
           MOVE FUNCTION TRIM(AR-RATE-ED) TO SQLIO-PARM-VAL(12)
           MOVE RA-GROSS TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(13)
           MOVE RA-GROSS-GBP TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(14)
           MOVE RA-BANK-FEE TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(15)
      *>   CR2027 - AllocatedAmount and Status are literals, so the
      *>   markers run one and two behind the column list
           MOVE RA-GROSS TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(16)
           MOVE RA-BANK-REF TO SQLIO-PARM-VAL(17)
           MOVE RA-EXTERNAL-REF TO SQLIO-PARM-VAL(18)
           MOVE RA-CONFIRM-STATE TO SQLIO-PARM-VAL(19)
           MOVE AR-CORREL TO SQLIO-PARM-VAL(20)
           MOVE AR-USERID TO SQLIO-PARM-VAL(21)
           MOVE 21 TO SQLIO-PARM-CNT
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 21
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1300-EXIT
           END-IF
           COMPUTE RA-RECEIPT-ID = FUNCTION NUMVAL(WS-NEW-ID)
           MOVE RA-GROSS TO RA-UNALLOCATED.
       1300-EXIT.
           EXIT.

      *>   confirmed money sits on the account as an unapplied credit
       1400-OPEN-ITEM SECTION.
       1400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT ar.OpenItem (CompanyCode, "
                  "CustomerCode, ItemType, DocumentNumber, "
                  "DocumentDate, DueDate, PostingDate, FiscalYear, "
                  "PeriodNo, CurrencyCode, FxRate, OriginalAmount, "
                  "OriginalAmountGBP, ResidualAmount, "
                  "ResidualAmountGBP, SourceReceiptId, CreatedBy) "
                  "VALUES (?, ?, 'RECEIPT', ?, ?, ?, ?, NULLIF(?,0), "
                  "NULLIF(?,0), ?, ?, ?, ?, ?, ?, ?, ?); "
                  "SELECT CONVERT(varchar(24), SCOPE_IDENTITY()), ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 16 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           IF RA-CUSTOMER = SPACES
               MOVE RA-PAYER TO SQLIO-PARM-VAL(2)
           ELSE
               MOVE RA-CUSTOMER TO SQLIO-PARM-VAL(2)
           END-IF
           MOVE RA-RECEIPT-NO TO SQLIO-PARM-VAL(3)
           MOVE RA-RECEIPT-DATE TO SQLIO-PARM-VAL(4)
           MOVE RA-RECEIPT-DATE TO SQLIO-PARM-VAL(5)
           MOVE RA-POSTING-DATE TO SQLIO-PARM-VAL(6)
           MOVE PC-FISCAL-YEAR TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(7)
           MOVE PC-PERIOD-NO TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(8)
           MOVE RA-CURRENCY TO SQLIO-PARM-VAL(9)
           MOVE RA-FX-RATE TO AR-RATE-ED
           MOVE FUNCTION TRIM(AR-RATE-ED) TO SQLIO-PARM-VAL(10)
           COMPUTE AR-AMT1 = 0 - RA-GROSS
           MOVE AR-AMT1 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(11)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(13)
           COMPUTE AR-AMT2 = 0 - RA-GROSS-GBP
           MOVE AR-AMT2 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(12)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(14)
           MOVE RA-RECEIPT-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(15)
           MOVE AR-USERID TO SQLIO-PARM-VAL(16)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 16
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1400-EXIT
           END-IF
           COMPUTE RA-SOURCE-ITEM-ID = FUNCTION NUMVAL(WS-NEW-ID)
      *>   the credit just written IS the source a later ALLOCATE will
      *>   settle from, so name it whole - id, document and type.  A
      *>   half-stated source leaves the caller's area naming one
      *>   document by id and another by number, and 2050-READ-SOURCE
      *>   then holds neither (RCP0020).
           MOVE RA-RECEIPT-NO TO RA-SOURCE-DOC
           MOVE "RECEIPT" TO RA-SOURCE-TYPE
           MOVE RA-GROSS TO RA-SOURCE-AVAIL.
       1400-EXIT.
           EXIT.

      *>   DR bank, CR trade debtors.  The bank fee is separate and is
      *>   never netted off the customer's settlement (AR-014).
       1500-POST-CASH SECTION.
       1500-START.
           INITIALIZE WS-POSTGL-CA
           MOVE "POST" TO CA-FUNCTION OF WS-POSTGL-CA
           MOVE AR-CO TO CA-COMPANY OF WS-POSTGL-CA
           MOVE AR-USERID TO CA-USER OF WS-POSTGL-CA
           MOVE AR-CORREL TO CA-CORRELATION-ID OF WS-POSTGL-CA
           MOVE RA-POSTING-DATE TO CA-BUSINESS-DATE OF WS-POSTGL-CA
           IF RA-METHOD = "DD"
               MOVE "AR.DDCOLLECT" TO PG-EVENT-CODE
           ELSE
               MOVE "AR.RECEIPT" TO PG-EVENT-CODE
           END-IF
           MOVE "AR" TO PG-JOURNAL-TYPE
           MOVE "AR" TO PG-SOURCE-MODULE
           MOVE RA-RECEIPT-NO TO PG-SOURCE-REF
           MOVE RA-RECEIPT-DATE TO PG-DOC-DATE
           MOVE RA-POSTING-DATE TO PG-POSTING-DATE
           MOVE RA-CURRENCY TO PG-CURRENCY
           MOVE RA-FX-RATE TO PG-FX-RATE
           MOVE "N" TO PG-OWN-TRAN
           MOVE SPACES TO PG-DESCRIPTION
           STRING "Customer receipt " FUNCTION TRIM(RA-RECEIPT-NO)
                  " " FUNCTION TRIM(RA-PAYER-NAME)
               DELIMITED BY SIZE INTO PG-DESCRIPTION
           MOVE 1 TO WS-LEG
           MOVE "BANK" TO PG-G-LEG-CODE(1)
           MOVE RA-GROSS TO PG-G-DEBIT(1)
           MOVE RA-BANK-ACCOUNT TO PG-G-BANK-ACCT(1)
           MOVE RA-BANK-REF TO PG-G-NARRATIVE(1)
           ADD 1 TO WS-LEG
           MOVE "DEBTOR" TO PG-G-LEG-CODE(WS-LEG)
           MOVE RA-GROSS TO PG-G-CREDIT(WS-LEG)
           MOVE "AR" TO PG-G-SUBLEDGER(WS-LEG)
           MOVE RA-CUSTOMER TO PG-G-SUBREF(WS-LEG)
           MOVE RA-RECEIPT-NO TO PG-G-NARRATIVE(WS-LEG)
           MOVE WS-LEG TO PG-LEG-CNT
           CALL "ARPOSTGL" USING WS-POSTGL-CA
           IF CA-RETURN-CODE OF WS-POSTGL-CA > 4
               MOVE CA-RETURN-CODE OF WS-POSTGL-CA TO AR-RC
               MOVE CA-REASON-CODE OF WS-POSTGL-CA TO AR-REASON
               MOVE CA-MESSAGE OF WS-POSTGL-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 1500-EXIT
           END-IF
           MOVE PG-JOURNAL-NO TO RA-JOURNAL-NO
           PERFORM 1550-LINK-JOURNAL.
       1500-EXIT.
           EXIT.

       1550-LINK-JOURNAL SECTION.
       1550-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.Receipt SET JournalId = (SELECT TOP (1) "
                  "j.JournalId FROM fin.Journal j WHERE "
                  "j.CompanyCode = ? AND j.JournalNumber = ?) "
                  "WHERE ReceiptId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE RA-JOURNAL-NO TO SQLIO-PARM-VAL(2)
           MOVE RA-RECEIPT-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 3
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1550-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW.
       1550-EXIT.
           EXIT.

      *>   unidentified money stays in an owned exception queue
       1600-UNIDENTIFIED-QUEUE SECTION.
       1600-START.
           INITIALIZE WS-EXCP-CA
           MOVE "RAISE" TO CA-FUNCTION OF WS-EXCP-CA
           MOVE AR-CO TO CA-COMPANY OF WS-EXCP-CA
           MOVE AR-USERID TO CA-USER OF WS-EXCP-CA
           MOVE RA-POSTING-DATE TO CA-BUSINESS-DATE OF WS-EXCP-CA
           MOVE "AR" TO EX-MODULE
           MOVE "WARNING" TO EX-SEVERITY
           MOVE "UNIDENTIFIED-CASH" TO EX-CATEGORY
           MOVE SPACES TO EX-MESSAGE-TEXT
           MOVE RA-GROSS-GBP TO AR-MONEY-UK
           STRING "Unidentified receipt of GBP "
                  FUNCTION TRIM(AR-MONEY-UK)
                  " awaiting identification"
               DELIMITED BY SIZE INTO EX-MESSAGE-TEXT
           MOVE "RECEIPT" TO EX-DOCUMENT-TYPE
           MOVE RA-RECEIPT-NO TO EX-DOCUMENT-REF
           MOVE "ARCLERK" TO EX-OWNER-ROLE
           MOVE 2 TO EX-PRIORITY
           MOVE RA-GROSS-GBP TO EX-AMOUNT-GBP
           CALL "EXCPRAIS" USING WS-EXCP-CA
           MOVE EX-EXCEPTION-ID TO RA-EXCEPTION-ID.
       1600-EXIT.
           EXIT.

      *>   CONFIRM - unconfirmed evidence becomes funds
       1800-CONFIRM-RECEIPT SECTION.
       1800-START.
           PERFORM 1050-PERMISSION
           IF AR-RC-BAD
               GO TO 1800-EXIT
           END-IF
           PERFORM 1850-READ-RECEIPT
           IF AR-RC-BAD
               GO TO 1800-EXIT
           END-IF
           IF WS-S-CONFIRM = "CONFIRMED"
               MOVE 4 TO AR-RC
               MOVE "RCP0010" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Receipt " FUNCTION TRIM(RA-RECEIPT-NO)
                      " is already confirmed"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 1800-EXIT
           END-IF
           PERFORM 1100-PERIOD
           IF AR-RC-BAD
               GO TO 1800-EXIT
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "Y" TO WS-TRAN-OPEN
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.Receipt SET ConfirmationState = "
                  "'CONFIRMED', ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() WHERE ReceiptId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-USERID TO SQLIO-PARM-VAL(1)
           MOVE RA-RECEIPT-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               PERFORM 9500-ROLLBACK
               GO TO 1800-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "CONFIRMED" TO RA-CONFIRM-STATE
           PERFORM 1400-OPEN-ITEM
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1800-EXIT
           END-IF
           PERFORM 1500-POST-CASH
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1800-EXIT
           END-IF
           MOVE "COMT" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "N" TO WS-TRAN-OPEN
           MOVE SPACES TO AR-MSG
           STRING "Receipt " FUNCTION TRIM(RA-RECEIPT-NO)
                  " confirmed and posted to cash"
               DELIMITED BY SIZE INTO AR-MSG.
       1800-EXIT.
           EXIT.

       1850-READ-RECEIPT SECTION.
       1850-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), r.ReceiptId), "
                  "ISNULL(r.CustomerCode,''), r.CurrencyCode, "
                  "CONVERT(varchar(24), r.FxRate), "
                  "CONVERT(varchar(24), r.UnallocatedAmount), "
                  "CONVERT(varchar(24), r.GrossReceivedGBP), "
                  "r.PaymentMethod, r.ReceiptNumber, "
                  "r.ConfirmationState, r.BankAccountCode "
                  "FROM ar.Receipt r WHERE r.CompanyCode = ? "
                  "AND (r.ReceiptNumber = ? OR r.ReceiptId = "
                  "CASE WHEN ? = '' THEN -1 ELSE CONVERT(bigint, ?) "
                  "END)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE RA-RECEIPT-NO TO SQLIO-PARM-VAL(2)
           IF RA-RECEIPT-ID = 0
               MOVE SPACES TO SQLIO-PARM-VAL(3)
               MOVE SPACES TO SQLIO-PARM-VAL(4)
           ELSE
               MOVE RA-RECEIPT-ID TO AR-INT-ED
               MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(3)
               MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(4)
           END-IF
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 4
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
           MOVE 30 TO SQLIO-COL-WIDTH(8)
           MOVE 12 TO SQLIO-COL-WIDTH(9)
           MOVE 12 TO SQLIO-COL-WIDTH(10)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SRC
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1850-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "RCP0011" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "That receipt is not held for this company"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 1850-EXIT
           END-IF
           COMPUTE RA-RECEIPT-ID = FUNCTION NUMVAL(WS-S-ID)
           MOVE WS-S-DOCNO TO RA-RECEIPT-NO
           MOVE WS-S-CUSTOMER TO RA-CUSTOMER
           MOVE WS-S-CURRENCY TO RA-CURRENCY
           COMPUTE RA-FX-RATE = FUNCTION NUMVAL(WS-S-RATE)
           COMPUTE RA-GROSS = FUNCTION NUMVAL(WS-S-RESIDUAL)
           COMPUTE RA-GROSS-GBP = FUNCTION NUMVAL(WS-S-RESID-GBP)
           MOVE WS-S-TYPE TO RA-METHOD
           MOVE WS-S-CONTROL TO RA-BANK-ACCOUNT.
       1850-EXIT.
           EXIT.

       1950-IDEMPOTENT-END SECTION.
       1950-START.
           INITIALIZE WS-IDEM-CA
           MOVE "END" TO CA-FUNCTION OF WS-IDEM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-IDEM-CA
           MOVE AR-USERID TO CA-USER OF WS-IDEM-CA
           MOVE AR-CORREL TO CA-CORRELATION-ID OF WS-IDEM-CA
           MOVE RA-POSTING-DATE TO CA-BUSINESS-DATE OF WS-IDEM-CA
           MOVE "ar.Receipt.Post" TO ID-OPERATION
           MOVE "COMPLETED" TO ID-STATUS
           MOVE RA-RECEIPT-NO TO ID-RESULT-REF
           MOVE "Customer receipt posted" TO ID-RESULT-MESSAGE
           CALL "IDEMPOT" USING WS-IDEM-CA.
       1950-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   AR-010 / AR-011 / AR-012 / AR-019
      *>   ALLOCATE applies a receipt, credit or deposit to eligible
      *>   open items.  Neither the source available amount nor the
      *>   target outstanding amount may be exceeded.  PREVIEW does
      *>   the same arithmetic and writes nothing.
       2000-ALLOCATE SECTION.
       2000-START.
           PERFORM 1050-PERMISSION
           IF AR-RC-STOP
               GO TO 2000-EXIT
           END-IF
           PERFORM 2050-READ-SOURCE
           IF AR-RC-STOP
               GO TO 2000-EXIT
           END-IF
           IF WS-PREVIEW = "N"
               PERFORM 1100-PERIOD
               IF AR-RC-STOP
                   GO TO 2000-EXIT
               END-IF
               MOVE "BEGN" TO SQLIO-FUNC
               MOVE 0 TO SQLIO-COL-CNT
               CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
               MOVE "Y" TO WS-TRAN-OPEN
           END-IF
           MOVE RA-SOURCE-AVAIL TO WS-SRC-LEFT
           PERFORM VARYING AR-I FROM 1 BY 1
               UNTIL AR-I > RA-ALLOC-CNT OR AR-RC-STOP
               PERFORM 2100-ONE-ALLOCATION
           END-PERFORM
           IF AR-RC-STOP
               PERFORM 9500-ROLLBACK
               GO TO 2000-EXIT
           END-IF
           MOVE WS-SRC-LEFT TO RA-RESIDUAL
           IF WS-PREVIEW = "Y"
               PERFORM 2800-PREVIEW-MESSAGE
               GO TO 2000-EXIT
           END-IF
           PERFORM 2600-POST-ADJUSTMENTS
           IF AR-RC-STOP
               PERFORM 9500-ROLLBACK
               GO TO 2000-EXIT
           END-IF
           PERFORM 2700-UPDATE-SOURCE
           IF AR-RC-STOP
               PERFORM 9500-ROLLBACK
               GO TO 2000-EXIT
           END-IF
           MOVE "COMT" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "N" TO WS-TRAN-OPEN
           MOVE RA-TOT-APPLIED TO AR-MONEY-UK
           MOVE WS-SRC-LEFT TO AR-MONEY-ED
           MOVE SPACES TO AR-MSG
           STRING "Allocated " FUNCTION TRIM(AR-MONEY-UK)
                  " from " FUNCTION TRIM(RA-SOURCE-DOC)
                  "; unapplied residual "
                  FUNCTION TRIM(AR-MONEY-ED)
               DELIMITED BY SIZE INTO AR-MSG.
       2000-EXIT.
           EXIT.

      *>   only posted, confirmed, usable money is an eligible source
       2050-READ-SOURCE SECTION.
       2050-START.
      *>   both identifiers are optional filters, so a caller that
      *>   names neither would be handed the first open item of the
      *>   company - a stranger's credit - and its customer, currency
      *>   and document written back into the caller's area.  Money is
      *>   always named before it is applied.
           IF RA-SOURCE-ITEM-ID = 0 AND RA-SOURCE-DOC = SPACES
               MOVE 16 TO AR-RC
               MOVE "RCP0023" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Name the receipt or credit the money is to "
                      "come from before it is applied"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 2050-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), o.OpenItemId), "
                  "o.CustomerCode, o.CurrencyCode, "
                  "CONVERT(varchar(24), o.FxRate), "
                  "CONVERT(varchar(24), -o.ResidualAmount), "
                  "CONVERT(varchar(24), -o.ResidualAmountGBP), "
                  "o.ItemType, o.DocumentNumber, "
                  "ISNULL((SELECT r.ConfirmationState FROM ar.Receipt"
                  " r WHERE r.ReceiptId = o.SourceReceiptId),"
                  "'CONFIRMED'), ISNULL((SELECT k.ArControlAccount "
                  "FROM ar.Customer k WHERE k.CompanyCode = "
                  "o.CompanyCode AND k.CustomerCode = o.CustomerCode)"
                  ",'') FROM ar.OpenItem o WHERE o.CompanyCode = ? "
                  "AND o.OpenItemId = CASE WHEN ? = '0' THEN "
                  "o.OpenItemId ELSE CONVERT(bigint, ?) END "
                  "AND (? = '' OR o.DocumentNumber = ?) "
                  "AND o.SettlementStatus <> 'REVERSED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE RA-SOURCE-ITEM-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(2)
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE RA-SOURCE-DOC TO SQLIO-PARM-VAL(4)
           MOVE RA-SOURCE-DOC TO SQLIO-PARM-VAL(5)
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
           MOVE 30 TO SQLIO-COL-WIDTH(8)
           MOVE 12 TO SQLIO-COL-WIDTH(9)
           MOVE 12 TO SQLIO-COL-WIDTH(10)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SRC
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2050-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "RCP0020" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The source receipt or credit is not held"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 2050-EXIT
           END-IF
           COMPUTE RA-SOURCE-ITEM-ID = FUNCTION NUMVAL(WS-S-ID)
           MOVE WS-S-DOCNO TO RA-SOURCE-DOC
           MOVE WS-S-CUSTOMER TO RA-CUSTOMER
           MOVE WS-S-CURRENCY TO RA-CURRENCY
           COMPUTE WS-SRC-RATE = FUNCTION NUMVAL(WS-S-RATE)
           COMPUTE RA-SOURCE-AVAIL = FUNCTION NUMVAL(WS-S-RESIDUAL)
           MOVE WS-S-TYPE TO RA-SOURCE-TYPE
           IF WS-S-CONFIRM NOT = "CONFIRMED"
               MOVE 8 TO AR-RC
               MOVE "RCP0021" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Unconfirmed evidence is not funds and cannot "
                      "settle an invoice"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 2050-EXIT
           END-IF
           IF RA-SOURCE-AVAIL <= 0
               MOVE 8 TO AR-RC
               MOVE "RCP0022" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "There is nothing left to allocate from "
                      FUNCTION TRIM(RA-SOURCE-DOC)
                   DELIMITED BY SIZE INTO AR-MSG
           END-IF.
       2050-EXIT.
           EXIT.

       2100-ONE-ALLOCATION SECTION.
       2100-START.
           MOVE SPACES TO RA-A-MESSAGE(AR-I)
           MOVE 0 TO RA-A-FX-REALISED(AR-I)
           MOVE 0 TO RA-A-ALLOC-ID(AR-I)
           PERFORM 2150-READ-TARGET
           IF AR-RC-STOP
               GO TO 2100-EXIT
           END-IF
           PERFORM 2200-CHECK-SAME-DEBTOR
           IF AR-RC-STOP
               GO TO 2100-EXIT
           END-IF
           PERFORM 2250-CROSS-CURRENCY
           IF AR-RC-STOP
               GO TO 2100-EXIT
           END-IF
           PERFORM 2300-CHECK-DISCOUNT
           IF AR-RC-STOP
               GO TO 2100-EXIT
           END-IF
           PERFORM 2350-CHECK-WRITEOFF
           IF AR-RC-STOP
               GO TO 2100-EXIT
           END-IF
           PERFORM 2400-CHECK-LIMITS
           IF AR-RC-STOP
               GO TO 2100-EXIT
           END-IF
           PERFORM 2450-CLASSIFY
           IF WS-PREVIEW = "Y"
               GO TO 2100-EXIT
           END-IF
           PERFORM 2500-WRITE-ALLOCATION
           IF AR-RC-STOP
               GO TO 2100-EXIT
           END-IF
           PERFORM 2550-UPDATE-TARGET.
       2100-EXIT.
           EXIT.

       2150-READ-TARGET SECTION.
       2150-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), o.OpenItemId), "
                  "o.CustomerCode, o.CurrencyCode, "
                  "CONVERT(varchar(24), o.FxRate), "
                  "CONVERT(varchar(24), o.ResidualAmount), "
                  "CONVERT(varchar(24), o.ResidualAmountGBP), "
                  "o.ItemType, o.DocumentNumber, "
                  "ISNULL(CONVERT(char(8), o.DiscountDueDate, 112),"
                  "''), CONVERT(varchar(24), "
                  "ISNULL(o.DiscountAmount,0)), "
                  "CONVERT(char(8), o.DueDate, 112), "
                  "CONVERT(char(1), o.IsDisputed) "
                  "FROM ar.OpenItem o WHERE o.CompanyCode = ? "
                  "AND (o.OpenItemId = CASE WHEN ? = '0' THEN "
                  "o.OpenItemId ELSE CONVERT(bigint, ?) END) "
                  "AND (? = '' OR o.DocumentNumber = ?) "
                  "AND o.SettlementStatus <> 'REVERSED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE RA-A-ITEM-ID(AR-I) TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(2)
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE RA-A-DOC-NO(AR-I) TO SQLIO-PARM-VAL(4)
           MOVE RA-A-DOC-NO(AR-I) TO SQLIO-PARM-VAL(5)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 5
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE 12 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 3 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 24 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 12 TO SQLIO-COL-WIDTH(7)
           MOVE 30 TO SQLIO-COL-WIDTH(8)
           MOVE 10 TO SQLIO-COL-WIDTH(9)
           MOVE 24 TO SQLIO-COL-WIDTH(10)
           MOVE 10 TO SQLIO-COL-WIDTH(11)
           MOVE 4 TO SQLIO-COL-WIDTH(12)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TGT
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2150-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "RCP0023" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Target open item "
                      FUNCTION TRIM(RA-A-DOC-NO(AR-I))
                      " is not held"
                   DELIMITED BY SIZE INTO AR-MSG
               MOVE AR-MSG TO RA-A-MESSAGE(AR-I)
               ADD 1 TO AR-MSG-CNT
               GO TO 2150-EXIT
           END-IF
           COMPUTE RA-A-ITEM-ID(AR-I) = FUNCTION NUMVAL(WS-T-ID)
           MOVE WS-T-DOCNO TO RA-A-DOC-NO(AR-I)
           MOVE WS-T-CURRENCY TO RA-A-CURRENCY(AR-I)
           COMPUTE WS-TGT-RATE = FUNCTION NUMVAL(WS-T-RATE)
           COMPUTE RA-A-RESID-BEFORE(AR-I) =
               FUNCTION NUMVAL(WS-T-RESIDUAL).
       2150-EXIT.
           EXIT.

      *>   no automatic offset against another customer's balance
       2200-CHECK-SAME-DEBTOR SECTION.
       2200-START.
           IF WS-T-CUSTOMER = WS-S-CUSTOMER
               GO TO 2200-EXIT
           END-IF
           MOVE 8 TO AR-RC
           MOVE "RCP0024" TO AR-REASON
           MOVE SPACES TO AR-MSG
           STRING "Source " FUNCTION TRIM(RA-SOURCE-DOC)
                  " belongs to " FUNCTION TRIM(WS-S-CUSTOMER)
                  " and cannot settle a debt of "
                  FUNCTION TRIM(WS-T-CUSTOMER)
               DELIMITED BY SIZE INTO AR-MSG
           MOVE AR-MSG TO RA-A-MESSAGE(AR-I)
           ADD 1 TO AR-MSG-CNT.
       2200-EXIT.
           EXIT.

      *>   AR-019 - cross currency allocation needs an explicit
      *>   conversion instruction, never equal number matching
       2250-CROSS-CURRENCY SECTION.
       2250-START.
           MOVE "N" TO AR-DONE
           IF RA-A-CURRENCY(AR-I) = RA-CURRENCY
               GO TO 2250-EXIT
           END-IF
           IF RA-CROSS-CCY-RATE <= 0
               MOVE 8 TO AR-RC
               MOVE "RCP0025" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Allocating "
                      FUNCTION TRIM(RA-CURRENCY) " money to a "
                      FUNCTION TRIM(RA-A-CURRENCY(AR-I))
                      " item needs an explicit conversion instruction"
                   DELIMITED BY SIZE INTO AR-MSG
               MOVE AR-MSG TO RA-A-MESSAGE(AR-I)
               ADD 1 TO AR-MSG-CNT
               GO TO 2250-EXIT
           END-IF
           MOVE "Y" TO AR-DONE.
       2250-EXIT.
           EXIT.

      *>   AR-012 - a settlement discount is only valid inside the
      *>   stated terms plus the approved receipt timing grace (D-22)
       2300-CHECK-DISCOUNT SECTION.
       2300-START.
           IF RA-A-DISCOUNT(AR-I) = 0
               GO TO 2300-EXIT
           END-IF
           IF WS-T-DISC-DATE = SPACES
               MOVE 8 TO AR-RC
               MOVE "RCP0026" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "No prompt payment discount was offered on "
                      FUNCTION TRIM(RA-A-DOC-NO(AR-I))
                   DELIMITED BY SIZE INTO AR-MSG
               MOVE AR-MSG TO RA-A-MESSAGE(AR-I)
               ADD 1 TO AR-MSG-CNT
               GO TO 2300-EXIT
           END-IF
           MOVE WS-T-DISC-DATE(1:8) TO AR-NUM-DATE
           COMPUTE AR-DAYS = RA-EFFECTIVE-DATE - AR-NUM-DATE
           IF AR-DAYS > RA-DISC-GRACE-DAYS
               MOVE 8 TO AR-RC
               MOVE "RCP0027" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The discount period on "
                      FUNCTION TRIM(RA-A-DOC-NO(AR-I))
                      " expired on " WS-T-DISC-DATE(7:2) "/"
                      WS-T-DISC-DATE(5:2) "/" WS-T-DISC-DATE(1:4)
                   DELIMITED BY SIZE INTO AR-MSG
               MOVE AR-MSG TO RA-A-MESSAGE(AR-I)
               ADD 1 TO AR-MSG-CNT
           END-IF.
       2300-EXIT.
           EXIT.

      *>   D-22 - up to GBP 5.00 per receipt may be written off by the
      *>   AR clerk with a reason; larger write offs need authority
       2350-CHECK-WRITEOFF SECTION.
       2350-START.
           IF RA-A-WRITEOFF(AR-I) = 0
               GO TO 2350-EXIT
           END-IF
           IF RA-A-WO-REASON(AR-I) = SPACES
               MOVE 8 TO AR-RC
               MOVE "RCP0028" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A write off needs a reason; a short payment "
                      "is not silently written off"
                   DELIMITED BY SIZE INTO AR-MSG
               MOVE AR-MSG TO RA-A-MESSAGE(AR-I)
               ADD 1 TO AR-MSG-CNT
               GO TO 2350-EXIT
           END-IF
           IF RA-A-WRITEOFF(AR-I) > RA-WRITEOFF-LIMIT
               INITIALIZE WS-PERM-CA
               MOVE "CHECK" TO CA-FUNCTION OF WS-PERM-CA
               MOVE AR-CO TO CA-COMPANY OF WS-PERM-CA
               MOVE AR-SITE TO CA-SITE OF WS-PERM-CA
               MOVE AR-USERID TO CA-USER OF WS-PERM-CA
               MOVE RA-POSTING-DATE TO
                   CA-BUSINESS-DATE OF WS-PERM-CA
               MOVE "AR.WRITEOFF.APPROVE" TO PK-PERMISSION
               CALL "PERMCHK" USING WS-PERM-CA
               IF CA-RETURN-CODE OF WS-PERM-CA > 4
                   MOVE 6 TO AR-RC
                   MOVE "RCP0029" TO AR-REASON
                   MOVE RA-WRITEOFF-LIMIT TO AR-MONEY-UK
                   MOVE SPACES TO AR-MSG
                   STRING "A write off above GBP "
                          FUNCTION TRIM(AR-MONEY-UK)
                          " needs an authorised approval (D-22)"
                       DELIMITED BY SIZE INTO AR-MSG
                   MOVE AR-MSG TO RA-A-MESSAGE(AR-I)
                   ADD 1 TO AR-MSG-CNT
               END-IF
           END-IF.
       2350-EXIT.
           EXIT.

      *>   neither side of an allocation may be exceeded
       2400-CHECK-LIMITS SECTION.
       2400-START.
           IF RA-A-APPLY(AR-I) <= 0
               MOVE 8 TO AR-RC
               MOVE "RCP0030" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Each allocation needs a positive applied "
                      "amount" DELIMITED BY SIZE INTO AR-MSG
               MOVE AR-MSG TO RA-A-MESSAGE(AR-I)
               ADD 1 TO AR-MSG-CNT
               GO TO 2400-EXIT
           END-IF
           IF RA-A-APPLY(AR-I) > WS-SRC-LEFT
               MOVE 8 TO AR-RC
               MOVE "RCP0031" TO AR-REASON
               MOVE WS-SRC-LEFT TO AR-MONEY-UK
               MOVE SPACES TO AR-MSG
               STRING "The allocation exceeds the available amount "
                      "of " FUNCTION TRIM(AR-MONEY-UK) " on "
                      FUNCTION TRIM(RA-SOURCE-DOC)
                   DELIMITED BY SIZE INTO AR-MSG
               MOVE AR-MSG TO RA-A-MESSAGE(AR-I)
               ADD 1 TO AR-MSG-CNT
               GO TO 2400-EXIT
           END-IF
           COMPUTE AR-AMT1 = RA-A-APPLY(AR-I)
                           + RA-A-DISCOUNT(AR-I)
                           + RA-A-WRITEOFF(AR-I)
           IF AR-AMT1 > RA-A-RESID-BEFORE(AR-I)
               MOVE 8 TO AR-RC
               MOVE "RCP0032" TO AR-REASON
               MOVE RA-A-RESID-BEFORE(AR-I) TO AR-MONEY-UK
               MOVE SPACES TO AR-MSG
               STRING "Over allocation: "
                      FUNCTION TRIM(RA-A-DOC-NO(AR-I))
                      " has only " FUNCTION TRIM(AR-MONEY-UK)
                      " outstanding"
                   DELIMITED BY SIZE INTO AR-MSG
               MOVE AR-MSG TO RA-A-MESSAGE(AR-I)
               ADD 1 TO AR-MSG-CNT
           END-IF.
       2400-EXIT.
           EXIT.

      *>   classify, value in sterling and work out the realised FX
       2450-CLASSIFY SECTION.
       2450-START.
           COMPUTE AR-AMT1 = RA-A-APPLY(AR-I)
                           + RA-A-DISCOUNT(AR-I)
                           + RA-A-WRITEOFF(AR-I)
           COMPUTE RA-A-RESID-AFTER(AR-I) =
               RA-A-RESID-BEFORE(AR-I) - AR-AMT1
           EVALUATE TRUE
               WHEN RA-A-WRITEOFF(AR-I) NOT = 0
                   MOVE "WRITEOFF" TO RA-A-KIND(AR-I)
               WHEN RA-A-DISCOUNT(AR-I) NOT = 0
                   MOVE "DISCOUNT" TO RA-A-KIND(AR-I)
               WHEN AR-DONE = "Y"
                   MOVE "CROSSCCY" TO RA-A-KIND(AR-I)
               WHEN RA-A-RESID-AFTER(AR-I) = 0
                   MOVE "EXACT" TO RA-A-KIND(AR-I)
               WHEN OTHER
                   MOVE "PARTIAL" TO RA-A-KIND(AR-I)
           END-EVALUATE
           IF WS-SRC-RATE > 0
               COMPUTE WS-APPLY-GBP ROUNDED =
                   RA-A-APPLY(AR-I) / WS-SRC-RATE
               COMPUTE WS-DISC-GBP ROUNDED =
                   RA-A-DISCOUNT(AR-I) / WS-SRC-RATE
               COMPUTE WS-WO-GBP ROUNDED =
                   RA-A-WRITEOFF(AR-I) / WS-SRC-RATE
           ELSE
               MOVE RA-A-APPLY(AR-I) TO WS-APPLY-GBP
               MOVE RA-A-DISCOUNT(AR-I) TO WS-DISC-GBP
               MOVE RA-A-WRITEOFF(AR-I) TO WS-WO-GBP
           END-IF
      *>   realised exchange difference on a foreign invoice: the
      *>   sterling the debt was raised at against the sterling the
      *>   money actually came in at
           IF WS-TGT-RATE > 0 AND WS-SRC-RATE > 0
              AND WS-TGT-RATE NOT = WS-SRC-RATE
               COMPUTE AR-AMT2 ROUNDED =
                   RA-A-APPLY(AR-I) / WS-TGT-RATE
               COMPUTE RA-A-FX-REALISED(AR-I) =
                   AR-AMT2 - WS-APPLY-GBP
           END-IF
           SUBTRACT RA-A-APPLY(AR-I) FROM WS-SRC-LEFT
           ADD RA-A-APPLY(AR-I) TO RA-TOT-APPLIED
           ADD RA-A-DISCOUNT(AR-I) TO RA-TOT-DISCOUNT
           ADD RA-A-WRITEOFF(AR-I) TO RA-TOT-WRITEOFF
           ADD RA-A-FX-REALISED(AR-I) TO RA-TOT-FX.
       2450-EXIT.
           EXIT.

       2500-WRITE-ALLOCATION SECTION.
       2500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT ar.Allocation "
                  "(CompanyCode, CustomerCode, SourceType, "
                  "SourceOpenItemId, TargetOpenItemId, EffectiveDate,"
                  " CurrencyCode, AppliedAmount, AppliedAmountGBP, "
                  "DiscountAmountGBP, WriteOffAmountGBP, "
                  "WriteOffReason, RealisedFxGBP, IsCrossCurrency, "
                  "ConversionRate, Status, CreatedBy) VALUES "
                  "(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULLIF(?,''), ?,"
                  " ?, NULLIF(CONVERT(decimal(18,8),?),0), 'ACTIVE', "
                  "?); SELECT CONVERT(varchar(24), SCOPE_IDENTITY()),"
                  " ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 16 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE RA-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE RA-SOURCE-TYPE TO SQLIO-PARM-VAL(3)
           MOVE RA-SOURCE-ITEM-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE RA-A-ITEM-ID(AR-I) TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(5)
           MOVE RA-EFFECTIVE-DATE TO SQLIO-PARM-VAL(6)
           MOVE RA-CURRENCY TO SQLIO-PARM-VAL(7)
           MOVE RA-A-APPLY(AR-I) TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(8)
           MOVE WS-APPLY-GBP TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(9)
           MOVE WS-DISC-GBP TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(10)
           MOVE WS-WO-GBP TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(11)
           MOVE RA-A-WO-REASON(AR-I) TO SQLIO-PARM-VAL(12)
           MOVE RA-A-FX-REALISED(AR-I) TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(13)
           IF AR-DONE = "Y"
               MOVE "1" TO SQLIO-PARM-VAL(14)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(14)
           END-IF
           MOVE RA-CROSS-CCY-RATE TO AR-RATE-ED
           MOVE FUNCTION TRIM(AR-RATE-ED) TO SQLIO-PARM-VAL(15)
           MOVE AR-USERID TO SQLIO-PARM-VAL(16)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 16
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
               GO TO 2500-EXIT
           END-IF
           COMPUTE RA-A-ALLOC-ID(AR-I) = FUNCTION NUMVAL(WS-NEW-ID).
       2500-EXIT.
           EXIT.

       2550-UPDATE-TARGET SECTION.
       2550-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.OpenItem SET ResidualAmount = "
                  "ResidualAmount - ?, ResidualAmountGBP = "
                  "ResidualAmountGBP - ?, SettlementStatus = "
                  "CASE WHEN ResidualAmount - ? = 0 THEN 'SETTLED' "
                  "ELSE 'PARTIAL' END, WrittenOff = CASE WHEN "
      *>           CR2027 - parameters arrive as text, so the write
      *>           off test must convert before it compares
                  "CONVERT(decimal(18,2), ?) > 0 "
                  "THEN 1 ELSE WrittenOff END, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE OpenItemId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           COMPUTE AR-AMT1 = RA-A-APPLY(AR-I)
                           + RA-A-DISCOUNT(AR-I)
                           + RA-A-WRITEOFF(AR-I)
           MOVE AR-AMT1 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(1)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(3)
           COMPUTE AR-AMT2 = WS-APPLY-GBP + WS-DISC-GBP + WS-WO-GBP
           MOVE AR-AMT2 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(2)
           MOVE WS-WO-GBP TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(4)
           MOVE AR-USERID TO SQLIO-PARM-VAL(5)
           MOVE RA-A-ITEM-ID(AR-I) TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(6)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 6
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2550-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW.
       2550-EXIT.
           EXIT.

      *>   discounts, write offs and realised FX are the only parts of
      *>   an allocation that touch the nominal
       2600-POST-ADJUSTMENTS SECTION.
       2600-START.
           IF RA-TOT-DISCOUNT = 0 AND RA-TOT-WRITEOFF = 0
              AND RA-TOT-FX = 0
               GO TO 2600-EXIT
           END-IF
           INITIALIZE WS-POSTGL-CA
           MOVE "POST" TO CA-FUNCTION OF WS-POSTGL-CA
           MOVE AR-CO TO CA-COMPANY OF WS-POSTGL-CA
           MOVE AR-USERID TO CA-USER OF WS-POSTGL-CA
           MOVE AR-CORREL(1:30) TO WS-CORR-STEM
           MOVE SPACES TO WS-ALLOC-CORR
           STRING FUNCTION TRIM(WS-CORR-STEM) "-ALLOC"
               DELIMITED BY SIZE INTO WS-ALLOC-CORR
           MOVE WS-ALLOC-CORR TO CA-CORRELATION-ID OF WS-POSTGL-CA
           MOVE RA-POSTING-DATE TO CA-BUSINESS-DATE OF WS-POSTGL-CA
           MOVE "AR" TO PG-JOURNAL-TYPE
           MOVE "AR" TO PG-SOURCE-MODULE
           MOVE RA-SOURCE-DOC TO PG-SOURCE-REF
           MOVE RA-EFFECTIVE-DATE TO PG-DOC-DATE
           MOVE RA-POSTING-DATE TO PG-POSTING-DATE
           MOVE "GBP" TO PG-CURRENCY
           MOVE 1 TO PG-FX-RATE
           MOVE "N" TO PG-OWN-TRAN
           MOVE SPACES TO PG-DESCRIPTION
           STRING "Allocation adjustments for "
                  FUNCTION TRIM(RA-SOURCE-DOC)
               DELIMITED BY SIZE INTO PG-DESCRIPTION
           MOVE 0 TO WS-LEG
           IF RA-TOT-DISCOUNT NOT = 0
               MOVE "AR.DISCOUNT" TO PG-EVENT-CODE
               ADD 1 TO WS-LEG
               MOVE "DISCOUNT" TO PG-G-LEG-CODE(WS-LEG)
               MOVE RA-TOT-DISCOUNT TO PG-G-DEBIT(WS-LEG)
               MOVE "Settlement discount allowed"
                   TO PG-G-NARRATIVE(WS-LEG)
               ADD 1 TO WS-LEG
               MOVE "DEBTOR" TO PG-G-LEG-CODE(WS-LEG)
               MOVE RA-TOT-DISCOUNT TO PG-G-CREDIT(WS-LEG)
               MOVE "AR" TO PG-G-SUBLEDGER(WS-LEG)
               MOVE RA-CUSTOMER TO PG-G-SUBREF(WS-LEG)
           END-IF
           IF RA-TOT-WRITEOFF NOT = 0
               MOVE "AR.WRITEOFF" TO PG-EVENT-CODE
               ADD 1 TO WS-LEG
               MOVE "BADDEBT" TO PG-G-LEG-CODE(WS-LEG)
               MOVE RA-TOT-WRITEOFF TO PG-G-DEBIT(WS-LEG)
               MOVE "Receivable written off"
                   TO PG-G-NARRATIVE(WS-LEG)
               ADD 1 TO WS-LEG
               MOVE "DEBTOR" TO PG-G-LEG-CODE(WS-LEG)
               MOVE RA-TOT-WRITEOFF TO PG-G-CREDIT(WS-LEG)
               MOVE "AR" TO PG-G-SUBLEDGER(WS-LEG)
               MOVE RA-CUSTOMER TO PG-G-SUBREF(WS-LEG)
           END-IF
           IF RA-TOT-FX NOT = 0
               ADD 1 TO WS-LEG
               ADD 1 TO WS-LEG
               IF RA-TOT-FX > 0
                   MOVE "AR.FXGAIN" TO PG-EVENT-CODE
                   SUBTRACT 1 FROM WS-LEG
                   MOVE "DEBTOR" TO PG-G-LEG-CODE(WS-LEG)
                   MOVE RA-TOT-FX TO PG-G-DEBIT(WS-LEG)
                   MOVE "AR" TO PG-G-SUBLEDGER(WS-LEG)
                   MOVE RA-CUSTOMER TO PG-G-SUBREF(WS-LEG)
                   ADD 1 TO WS-LEG
                   MOVE "FX" TO PG-G-LEG-CODE(WS-LEG)
                   MOVE RA-TOT-FX TO PG-G-CREDIT(WS-LEG)
                   MOVE "Realised exchange gain"
                       TO PG-G-NARRATIVE(WS-LEG)
               ELSE
                   MOVE "AR.FXLOSS" TO PG-EVENT-CODE
                   SUBTRACT 1 FROM WS-LEG
                   MOVE "FX" TO PG-G-LEG-CODE(WS-LEG)
                   COMPUTE AR-AMT1 = 0 - RA-TOT-FX
                   MOVE AR-AMT1 TO PG-G-DEBIT(WS-LEG)
                   MOVE "Realised exchange loss"
                       TO PG-G-NARRATIVE(WS-LEG)
                   ADD 1 TO WS-LEG
                   MOVE "DEBTOR" TO PG-G-LEG-CODE(WS-LEG)
                   MOVE AR-AMT1 TO PG-G-CREDIT(WS-LEG)
                   MOVE "AR" TO PG-G-SUBLEDGER(WS-LEG)
                   MOVE RA-CUSTOMER TO PG-G-SUBREF(WS-LEG)
               END-IF
           END-IF
           MOVE WS-LEG TO PG-LEG-CNT
           CALL "ARPOSTGL" USING WS-POSTGL-CA
           IF CA-RETURN-CODE OF WS-POSTGL-CA > 4
               MOVE CA-RETURN-CODE OF WS-POSTGL-CA TO AR-RC
               MOVE CA-REASON-CODE OF WS-POSTGL-CA TO AR-REASON
               MOVE CA-MESSAGE OF WS-POSTGL-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 2600-EXIT
           END-IF
           MOVE PG-JOURNAL-NO TO RA-JOURNAL-NO.
       2600-EXIT.
           EXIT.

       2700-UPDATE-SOURCE SECTION.
       2700-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.OpenItem SET ResidualAmount = "
                  "ResidualAmount + ?, ResidualAmountGBP = "
                  "ResidualAmountGBP + ?, SettlementStatus = "
                  "CASE WHEN ResidualAmount + ? = 0 THEN 'SETTLED' "
                  "ELSE 'PARTIAL' END, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE OpenItemId = ?; "
                  "UPDATE ar.Receipt SET AllocatedAmount = "
                  "AllocatedAmount + ?, UnallocatedAmount = "
                  "UnallocatedAmount - ? WHERE CompanyCode = ? AND "
                  "ReceiptId = (SELECT o.SourceReceiptId FROM "
                  "ar.OpenItem o WHERE o.OpenItemId = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 9 TO SQLIO-PARM-CNT
           MOVE RA-TOT-APPLIED TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(1)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(3)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(6)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(7)
           IF WS-SRC-RATE > 0
               COMPUTE AR-AMT1 ROUNDED =
                   RA-TOT-APPLIED / WS-SRC-RATE
           ELSE
               MOVE RA-TOT-APPLIED TO AR-AMT1
           END-IF
           MOVE AR-AMT1 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(2)
           MOVE AR-USERID TO SQLIO-PARM-VAL(4)
           MOVE RA-SOURCE-ITEM-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(5)
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(9)
           MOVE AR-CO TO SQLIO-PARM-VAL(8)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 9
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2700-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE WS-SRC-LEFT TO RA-UNALLOCATED.
       2700-EXIT.
           EXIT.

       2800-PREVIEW-MESSAGE SECTION.
       2800-START.
           MOVE RA-TOT-APPLIED TO AR-MONEY-UK
           MOVE WS-SRC-LEFT TO AR-MONEY-ED
           MOVE SPACES TO AR-MSG
           STRING "Preview only: " FUNCTION TRIM(AR-MONEY-UK)
                  " would be applied leaving "
                  FUNCTION TRIM(AR-MONEY-ED) " unapplied"
               DELIMITED BY SIZE INTO AR-MSG.
       2800-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   AR-011 - an authorised correction preserves the original
      *>   allocation history and restores the residuals.  It never
      *>   reverses the receipt and never rewrites an earlier as-of
      *>   statement, because the reversal carries its own date.
       3000-UNALLOCATE SECTION.
       3000-START.
           PERFORM 1050-PERMISSION
           IF AR-RC-STOP
               GO TO 3000-EXIT
           END-IF
           IF RA-ALLOC-ID = 0
               MOVE 8 TO AR-RC
               MOVE "RCP0040" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "An allocation reference is required"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 3000-EXIT
           END-IF
           IF RA-CORRECT-REASON = SPACES
               MOVE 8 TO AR-RC
               MOVE "RCP0041" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "An allocation correction needs a reason"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 3000-EXIT
           END-IF
           IF RA-CORRECT-DATE = 0
               MOVE AR-BUSDATE TO RA-CORRECT-DATE
           END-IF
           PERFORM 3100-READ-ALLOCATION
           IF AR-RC-BAD
               GO TO 3000-EXIT
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "Y" TO WS-TRAN-OPEN
           PERFORM 3200-REVERSE-ALLOCATION
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 3000-EXIT
           END-IF
           MOVE "COMT" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "N" TO WS-TRAN-OPEN
           MOVE 1 TO RA-REVERSED-CNT
           MOVE RA-TOT-APPLIED TO AR-MONEY-UK
           MOVE SPACES TO AR-MSG
           STRING "Allocation reversed; "
                  FUNCTION TRIM(AR-MONEY-UK)
                  " is unapplied again. The receipt itself is "
                  "untouched."
               DELIMITED BY SIZE INTO AR-MSG.
       3000-EXIT.
           EXIT.

       3100-READ-ALLOCATION SECTION.
       3100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), a.SourceOpenItemId), "
                  "a.CustomerCode, a.CurrencyCode, "
                  "CONVERT(varchar(24), a.TargetOpenItemId), "
                  "CONVERT(varchar(24), a.AppliedAmount), "
                  "CONVERT(varchar(24), a.AppliedAmountGBP), "
                  "a.Status, '', '', '' FROM ar.Allocation a "
                  "WHERE a.AllocationId = ? AND a.CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE RA-ALLOC-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE AR-CO TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 10 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 3 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 24 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 12 TO SQLIO-COL-WIDTH(7)
           MOVE 30 TO SQLIO-COL-WIDTH(8)
           MOVE 12 TO SQLIO-COL-WIDTH(9)
           MOVE 12 TO SQLIO-COL-WIDTH(10)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SRC
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 3100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "RCP0042" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "That allocation is not held" DELIMITED BY SIZE
                   INTO AR-MSG
               GO TO 3100-EXIT
           END-IF
           IF WS-S-TYPE = "REVERSED"
               MOVE 8 TO AR-RC
               MOVE "RCP0043" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "That allocation has already been reversed"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 3100-EXIT
           END-IF
           COMPUTE RA-SOURCE-ITEM-ID = FUNCTION NUMVAL(WS-S-ID)
           MOVE WS-S-CUSTOMER TO RA-CUSTOMER
           MOVE WS-S-CURRENCY TO RA-CURRENCY
           COMPUTE AR-N = FUNCTION NUMVAL(WS-S-RATE)
           COMPUTE RA-TOT-APPLIED = FUNCTION NUMVAL(WS-S-RESIDUAL)
           COMPUTE AR-AMT3 = FUNCTION NUMVAL(WS-S-RESID-GBP).
       3100-EXIT.
           EXIT.

      *>   the original row stays ACTIVE in history with a REVERSED
      *>   status and a linked contra row carrying the correction date
       3200-REVERSE-ALLOCATION SECTION.
       3200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT ar.Allocation "
                  "(CompanyCode, CustomerCode, SourceType, "
                  "SourceOpenItemId, TargetOpenItemId, EffectiveDate,"
                  " CurrencyCode, AppliedAmount, AppliedAmountGBP, "
                  "WriteOffReason, Status, ReversalOfAllocationId, "
                  "ReversedAt, ReversedBy, CreatedBy) VALUES "
                  "(?, ?, 'ADJUST', ?, ?, ?, ?, ?, ?, ?, 'REVERSED', "
                  "?, SYSDATETIME(), ?, ?); "
                  "UPDATE ar.Allocation SET Status = 'REVERSED', "
                  "ReversedAt = SYSDATETIME(), ReversedBy = ? "
                  "WHERE AllocationId = ?; "
                  "UPDATE ar.OpenItem SET ResidualAmount = "
                  "ResidualAmount + ?, ResidualAmountGBP = "
                  "ResidualAmountGBP + ?, SettlementStatus = CASE "
                  "WHEN ABS(ResidualAmount + ?) >= "
                  "ABS(OriginalAmount) THEN 'OPEN' ELSE 'PARTIAL' "
                  "END, ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE OpenItemId = ?; "
                  "UPDATE ar.OpenItem SET ResidualAmount = "
                  "ResidualAmount - ?, ResidualAmountGBP = "
                  "ResidualAmountGBP - ?, SettlementStatus = CASE "
                  "WHEN ABS(ResidualAmount - ?) >= "
                  "ABS(OriginalAmount) THEN 'OPEN' ELSE 'PARTIAL' "
                  "END, ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE OpenItemId = ?; "
                  "SELECT CONVERT(varchar(24), ?), ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 25 TO SQLIO-PARM-CNT
      *>   1-12 the contra row, 13-14 the stamp on the original,
      *>   15-19 the debt that comes back, 20-24 the money that is
      *>   unapplied again, 25 the echo the driver fetches
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE RA-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE RA-SOURCE-ITEM-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(24)
           MOVE AR-N TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(19)
           MOVE RA-CORRECT-DATE TO SQLIO-PARM-VAL(5)
           MOVE RA-CURRENCY TO SQLIO-PARM-VAL(6)
           COMPUTE AR-AMT1 = 0 - RA-TOT-APPLIED
           MOVE AR-AMT1 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(7)
           COMPUTE AR-AMT2 = 0 - AR-AMT3
           MOVE AR-AMT2 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(8)
           MOVE RA-CORRECT-REASON TO SQLIO-PARM-VAL(9)
           MOVE RA-ALLOC-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(10)
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(14)
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(25)
           MOVE AR-USERID TO SQLIO-PARM-VAL(11)
           MOVE AR-USERID TO SQLIO-PARM-VAL(12)
           MOVE AR-USERID TO SQLIO-PARM-VAL(13)
           MOVE AR-USERID TO SQLIO-PARM-VAL(18)
           MOVE AR-USERID TO SQLIO-PARM-VAL(23)
           MOVE RA-TOT-APPLIED TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(15)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(17)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(20)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(22)
           MOVE AR-AMT3 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(16)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(21)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 25
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
           END-IF.
       3200-EXIT.
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
           MOVE "RCP0098" TO AR-REASON
           MOVE SPACES TO AR-MSG
           STRING "RCPTALOC does not support function ["
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
