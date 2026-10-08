      *>================================================================
      *> PROGRAM-ID : ARDEPOS
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : CUSTOMER DEPOSITS AND REFUNDS.  AN ADVANCE
      *>              CREATES A TAX POINT AND A DEPOSIT LIABILITY; IT
      *>              IS EITHER AN ELIGIBLE SEPARATE ADVANCE OR, AFTER
      *>              APPLICATION, PART OF RECEIVABLE SETTLEMENT -
      *>              NEVER BOTH.  A DEPOSIT CANNOT BE REFUNDED AFTER
      *>              THE SAME AMOUNT HAS BEEN APPLIED TO AN INVOICE.
      *> CALLED BY  : DEPOSIT ENTRY, SALES ORDER PREPAYMENT, ARINVPST
      *> INTERFACE  : CALL "ARDEPOS" USING WS-ARDEPOS-CA
      *>              COPYBOOK ARDEPOSCA
      *> CALLS      : SQLIO, PERMCHK, PERIODCK, NUMGEN, ARVATWRT,
      *>              ARPOSTGL
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-02-14 RJH  CR0366 CUSTOMER DEPOSIT CONTROL
      *> 2026-10-05 STERP VAT TAX POINT ON ADVANCE (SPEC F6)
      *>================================================================
      *> REQ: AR-013, AR-020
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARDEPOS.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 14/02/1994.
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
       COPY ARVATWRTCA REPLACING ==DFHCOMMAREA== BY ==WS-VATW-CA==.
       COPY ARPOSTGLCA REPLACING ==DFHCOMMAREA== BY ==WS-POSTGL-CA==.
       01  WS-DEP.
           05  WS-D-ID                 PIC X(24).
           05  WS-D-CUSTOMER           PIC X(20).
           05  WS-D-CURRENCY           PIC X(3).
           05  WS-D-ORIGINAL           PIC X(24).
           05  WS-D-RESIDUAL           PIC X(24).
           05  WS-D-DOCNO              PIC X(30).
           05  WS-D-VAT                PIC X(24).
           05  WS-D-STATUS             PIC X(12).
       01  WS-ID-ROW.
           05  WS-NEW-ID               PIC X(24).
           05  WS-ID2                  PIC X(24).
       01  WS-SW.
           05  WS-TRAN-OPEN            PIC X VALUE "N".
       LINKAGE SECTION.
       COPY ARDEPOSCA.
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
               WHEN "DEPOSIT"  PERFORM 1000-TAKE-DEPOSIT
               WHEN "ENQUIRE"  PERFORM 0300-READ-DEPOSIT
                               PERFORM 0400-REPORT-RESIDUAL
               WHEN "APPLY"    PERFORM 0300-READ-DEPOSIT
                               PERFORM 2000-APPLY-DEPOSIT
               WHEN "REFUND"   PERFORM 0300-READ-DEPOSIT
                               PERFORM 3000-REFUND-DEPOSIT
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           PERFORM 9900-SET-RESULT
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE AR-WORK
           INITIALIZE AR-CTL
           INITIALIZE WS-DEP
           MOVE "N" TO WS-TRAN-OPEN
           MOVE CA-FUNCTION OF DFHCOMMAREA TO AR-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO AR-CO
           MOVE CA-SITE OF DFHCOMMAREA TO AR-SITE
           MOVE CA-USER OF DFHCOMMAREA TO AR-USERID
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO AR-BUSDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO AR-CORREL
           MOVE "ARDEPOS" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO AR-RC AR-MSG-CNT
           MOVE SPACES TO AR-REASON AR-MSG
           MOVE "AR-013" TO AR-REQ-REF
           MOVE 0 TO DP-RESIDUAL DP-APPLIED-TOTAL DP-REFUNDED-TOTAL
           IF DP-DOC-DATE = 0
               MOVE AR-BUSDATE TO DP-DOC-DATE
           END-IF
           IF DP-POSTING-DATE = 0
               MOVE DP-DOC-DATE TO DP-POSTING-DATE
           END-IF
           IF DP-CURRENCY = SPACES
               MOVE "GBP" TO DP-CURRENCY
           END-IF
           IF DP-FX-RATE = 0
               MOVE 1 TO DP-FX-RATE
           END-IF
           IF DP-PURPOSE = SPACES
               MOVE "ADVANCE" TO DP-PURPOSE
           END-IF
           IF DP-CLEARED = SPACE OR DP-CLEARED = LOW-VALUE
               MOVE "Y" TO DP-CLEARED
           END-IF.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF AR-CO = SPACES OR DP-CUSTOMER = SPACES
               MOVE "DEP0001" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A company and a customer are required"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AR-FUNC = "DEPOSIT" AND DP-AMOUNT <= 0
               MOVE "DEP0002" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A deposit must be for a positive amount"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
           END-IF
           IF AR-FUNC NOT = "DEPOSIT" AND DP-DEPOSIT-REF = SPACES
               MOVE "DEP0003" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A deposit reference is required" DELIMITED BY
                   SIZE INTO AR-MSG
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

       0300-READ-DEPOSIT SECTION.
       0300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), o.OpenItemId), "
                  "o.CustomerCode, o.CurrencyCode, "
                  "CONVERT(varchar(24), -o.OriginalAmount), "
                  "CONVERT(varchar(24), -o.ResidualAmount), "
                  "o.DocumentNumber, CONVERT(varchar(24), "
                  "-o.VatAmountGBP), o.SettlementStatus "
                  "FROM ar.OpenItem o WHERE o.CompanyCode = ? "
                  "AND o.DocumentNumber = ? AND o.ItemType = "
                  "'DEPOSIT'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE DP-DEPOSIT-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 8 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 3 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 24 TO SQLIO-COL-WIDTH(5)
           MOVE 30 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 12 TO SQLIO-COL-WIDTH(8)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-DEP
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "DEP0004" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Deposit " FUNCTION TRIM(DP-DEPOSIT-REF)
                      " is not held" DELIMITED BY SIZE INTO AR-MSG
               GO TO 0300-EXIT
           END-IF
           COMPUTE DP-OPEN-ITEM-ID = FUNCTION NUMVAL(WS-D-ID)
           MOVE WS-D-CURRENCY TO DP-CURRENCY
           COMPUTE DP-AMOUNT = FUNCTION NUMVAL(WS-D-ORIGINAL)
           COMPUTE DP-RESIDUAL = FUNCTION NUMVAL(WS-D-RESIDUAL)
           COMPUTE DP-APPLIED-TOTAL = DP-AMOUNT - DP-RESIDUAL.
       0300-EXIT.
           EXIT.

       0400-REPORT-RESIDUAL SECTION.
       0400-START.
           IF AR-RC-BAD
               GO TO 0400-EXIT
           END-IF
           MOVE DP-RESIDUAL TO AR-MONEY-UK
           MOVE DP-APPLIED-TOTAL TO AR-MONEY-ED
           MOVE SPACES TO AR-MSG
           STRING "Deposit " FUNCTION TRIM(DP-DEPOSIT-REF)
                  ": applied " FUNCTION TRIM(AR-MONEY-ED)
                  ", residual " FUNCTION TRIM(AR-MONEY-UK)
               DELIMITED BY SIZE INTO AR-MSG.
       0400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   DEPOSIT - DR bank, CR customer deposit liability and
      *>   CR output VAT; the advance creates its own tax point
       1000-TAKE-DEPOSIT SECTION.
       1000-START.
           PERFORM 1050-PERMISSION
           IF AR-RC-BAD
               GO TO 1000-EXIT
           END-IF
           PERFORM 1100-PERIOD
           IF AR-RC-BAD
               GO TO 1000-EXIT
           END-IF
           PERFORM 1150-DRAW-NUMBER
           IF AR-RC-BAD
               GO TO 1000-EXIT
           END-IF
           PERFORM 1200-SPLIT-VAT
           IF AR-RC-BAD
               GO TO 1000-EXIT
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "Y" TO WS-TRAN-OPEN
           PERFORM 1300-WRITE-OPEN-ITEM
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           PERFORM 1400-POST-DEPOSIT
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           IF DP-VAT-GBP NOT = 0
               PERFORM 1500-VAT-RECORD
               IF AR-RC-BAD
                   PERFORM 9500-ROLLBACK
                   GO TO 1000-EXIT
               END-IF
           END-IF
           MOVE "COMT" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "N" TO WS-TRAN-OPEN
           MOVE DP-AMOUNT TO DP-RESIDUAL
           MOVE DP-AMOUNT TO AR-MONEY-UK
           MOVE DP-VAT-GBP TO AR-MONEY-ED
           MOVE SPACES TO AR-MSG
           STRING "Deposit " FUNCTION TRIM(DP-DEPOSIT-REF)
                  " of " FUNCTION TRIM(DP-CURRENCY) " "
                  FUNCTION TRIM(AR-MONEY-UK)
                  " received; output VAT GBP "
                  FUNCTION TRIM(AR-MONEY-ED) " accounted on the "
                  "advance"
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
           MOVE DP-POSTING-DATE TO CA-BUSINESS-DATE OF WS-PERM-CA
           MOVE "AR.RECEIPT.POST" TO PK-PERMISSION
           CALL "PERMCHK" USING WS-PERM-CA
           IF CA-RETURN-CODE OF WS-PERM-CA > 4
               MOVE 6 TO AR-RC
               MOVE "DEP0005" TO AR-REASON
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
           MOVE DP-POSTING-DATE TO CA-BUSINESS-DATE OF WS-PERIOD-CA
           MOVE DP-POSTING-DATE TO PC-POSTING-DATE
           MOVE DP-DOC-DATE TO PC-DOCUMENT-DATE
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

       1150-DRAW-NUMBER SECTION.
       1150-START.
           IF DP-DEPOSIT-REF NOT = SPACES
               GO TO 1150-EXIT
           END-IF
           INITIALIZE WS-NUM-CA
           MOVE "NEXT" TO CA-FUNCTION OF WS-NUM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-NUM-CA
           MOVE AR-USERID TO CA-USER OF WS-NUM-CA
           MOVE AR-CORREL TO CA-CORRELATION-ID OF WS-NUM-CA
           MOVE DP-DOC-DATE TO CA-BUSINESS-DATE OF WS-NUM-CA
           MOVE "ARRC" TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUM-CA
           IF CA-RETURN-CODE OF WS-NUM-CA > 4
               MOVE CA-RETURN-CODE OF WS-NUM-CA TO AR-RC
               MOVE CA-REASON-CODE OF WS-NUM-CA TO AR-REASON
               MOVE CA-MESSAGE OF WS-NUM-CA TO AR-MSG
               GO TO 1150-EXIT
           END-IF
           MOVE SPACES TO DP-DEPOSIT-REF
           STRING "DEP-" FUNCTION TRIM(NG-DOCUMENT-NUMBER)
               DELIMITED BY SIZE INTO DP-DEPOSIT-REF.
       1150-EXIT.
           EXIT.

      *>   the deposit is VAT inclusive; the liability is the net part
      *>   (spec F6: 240.00 = 200.00 liability + 40.00 output VAT)
       1200-SPLIT-VAT SECTION.
       1200-START.
           IF DP-CURRENCY = "GBP" OR DP-FX-RATE = 0
               MOVE DP-AMOUNT TO DP-AMOUNT-GBP
           ELSE
               COMPUTE DP-AMOUNT-GBP ROUNDED =
                   DP-AMOUNT / DP-FX-RATE
           END-IF
           MOVE 0 TO DP-VAT-GBP
           MOVE DP-AMOUNT-GBP TO DP-NET-GBP
           IF DP-VAT-CODE = SPACES
               GO TO 1200-EXIT
           END-IF
           INITIALIZE WS-VATW-CA
           MOVE "LOOKUP" TO CA-FUNCTION OF WS-VATW-CA
           MOVE AR-CO TO CA-COMPANY OF WS-VATW-CA
           MOVE AR-USERID TO CA-USER OF WS-VATW-CA
           MOVE DP-DOC-DATE TO CA-BUSINESS-DATE OF WS-VATW-CA
           MOVE DP-VAT-CODE TO VW-VAT-CODE
           MOVE DP-DOC-DATE TO VW-TAX-POINT
           MOVE "OUTPUT" TO VW-DIRECTION
           CALL "ARVATWRT" USING WS-VATW-CA
           IF CA-RETURN-CODE OF WS-VATW-CA > 6
               MOVE 8 TO AR-RC
               MOVE CA-REASON-CODE OF WS-VATW-CA TO AR-REASON
               MOVE CA-MESSAGE OF WS-VATW-CA TO AR-MSG
               GO TO 1200-EXIT
           END-IF
           MOVE VW-RATE-PCT TO DP-VAT-RATE
           IF VW-RATE-PCT = 0
               GO TO 1200-EXIT
           END-IF
           COMPUTE AR-AMT1 ROUNDED = DP-AMOUNT-GBP * VW-RATE-PCT
               / (100 + VW-RATE-PCT)
           MOVE AR-AMT1 TO DP-VAT-GBP
           COMPUTE DP-NET-GBP = DP-AMOUNT-GBP - DP-VAT-GBP.
       1200-EXIT.
           EXIT.

       1300-WRITE-OPEN-ITEM SECTION.
       1300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT ar.OpenItem (CompanyCode, "
                  "CustomerCode, ItemType, DocumentNumber, "
                  "DocumentDate, DueDate, PostingDate, FiscalYear, "
                  "PeriodNo, CurrencyCode, FxRate, OriginalAmount, "
                  "OriginalAmountGBP, ResidualAmount, "
                  "ResidualAmountGBP, VatAmountGBP, CreatedBy) "
                  "VALUES (?, ?, 'DEPOSIT', ?, ?, ?, ?, NULLIF(?,0), "
                  "NULLIF(?,0), ?, ?, ?, ?, ?, ?, ?, ?); "
                  "SELECT CONVERT(varchar(24), SCOPE_IDENTITY()), ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 16 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE DP-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE DP-DEPOSIT-REF TO SQLIO-PARM-VAL(3)
           MOVE DP-DOC-DATE TO SQLIO-PARM-VAL(4)
           MOVE DP-DOC-DATE TO SQLIO-PARM-VAL(5)
           MOVE DP-POSTING-DATE TO SQLIO-PARM-VAL(6)
           MOVE PC-FISCAL-YEAR TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(7)
           MOVE PC-PERIOD-NO TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(8)
           MOVE DP-CURRENCY TO SQLIO-PARM-VAL(9)
           MOVE DP-FX-RATE TO AR-RATE-ED
           MOVE FUNCTION TRIM(AR-RATE-ED) TO SQLIO-PARM-VAL(10)
           COMPUTE AR-AMT1 = 0 - DP-AMOUNT
           MOVE AR-AMT1 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(11)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(13)
           COMPUTE AR-AMT2 = 0 - DP-AMOUNT-GBP
           MOVE AR-AMT2 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(12)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(14)
           COMPUTE AR-AMT3 = 0 - DP-VAT-GBP
           MOVE AR-AMT3 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(15)
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
               GO TO 1300-EXIT
           END-IF
           COMPUTE DP-OPEN-ITEM-ID = FUNCTION NUMVAL(WS-NEW-ID).
       1300-EXIT.
           EXIT.

       1400-POST-DEPOSIT SECTION.
       1400-START.
           INITIALIZE WS-POSTGL-CA
           MOVE "POST" TO CA-FUNCTION OF WS-POSTGL-CA
           MOVE AR-CO TO CA-COMPANY OF WS-POSTGL-CA
           MOVE AR-USERID TO CA-USER OF WS-POSTGL-CA
           MOVE AR-CORREL TO CA-CORRELATION-ID OF WS-POSTGL-CA
           MOVE DP-POSTING-DATE TO CA-BUSINESS-DATE OF WS-POSTGL-CA
           MOVE "AR.DEPOSIT" TO PG-EVENT-CODE
           MOVE "AR" TO PG-JOURNAL-TYPE
           MOVE "AR" TO PG-SOURCE-MODULE
           MOVE DP-DEPOSIT-REF TO PG-SOURCE-REF
           MOVE DP-DOC-DATE TO PG-DOC-DATE
           MOVE DP-POSTING-DATE TO PG-POSTING-DATE
           MOVE "GBP" TO PG-CURRENCY
           MOVE 1 TO PG-FX-RATE
           MOVE "N" TO PG-OWN-TRAN
           MOVE SPACES TO PG-DESCRIPTION
           STRING "Customer deposit "
                  FUNCTION TRIM(DP-DEPOSIT-REF) " "
                  FUNCTION TRIM(DP-CUSTOMER)
               DELIMITED BY SIZE INTO PG-DESCRIPTION
           MOVE 3 TO PG-LEG-CNT
           MOVE "BANK" TO PG-G-LEG-CODE(1)
           MOVE DP-AMOUNT-GBP TO PG-G-DEBIT(1)
           MOVE DP-BANK-ACCOUNT TO PG-G-BANK-ACCT(1)
           MOVE DP-DEPOSIT-REF TO PG-G-NARRATIVE(1)
           MOVE "DEFERRED" TO PG-G-LEG-CODE(2)
           MOVE DP-NET-GBP TO PG-G-CREDIT(2)
           MOVE "AR" TO PG-G-SUBLEDGER(2)
           MOVE DP-CUSTOMER TO PG-G-SUBREF(2)
           MOVE "Customer deposit liability" TO PG-G-NARRATIVE(2)
           MOVE "VAT" TO PG-G-LEG-CODE(3)
           MOVE "2040" TO PG-G-ACCOUNT(3)
           MOVE DP-VAT-GBP TO PG-G-CREDIT(3)
           MOVE "Output VAT on advance" TO PG-G-NARRATIVE(3)
           MOVE DP-VAT-CODE TO PG-G-VAT-CODE(3)
           CALL "ARPOSTGL" USING WS-POSTGL-CA
           IF CA-RETURN-CODE OF WS-POSTGL-CA > 4
               MOVE CA-RETURN-CODE OF WS-POSTGL-CA TO AR-RC
               MOVE CA-REASON-CODE OF WS-POSTGL-CA TO AR-REASON
               MOVE CA-MESSAGE OF WS-POSTGL-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 1400-EXIT
           END-IF
           MOVE PG-JOURNAL-NO TO DP-JOURNAL-NO.
       1400-EXIT.
           EXIT.

       1500-VAT-RECORD SECTION.
       1500-START.
           INITIALIZE WS-VATW-CA
           MOVE "WRITE" TO CA-FUNCTION OF WS-VATW-CA
           MOVE AR-CO TO CA-COMPANY OF WS-VATW-CA
           MOVE AR-USERID TO CA-USER OF WS-VATW-CA
           MOVE DP-DOC-DATE TO CA-BUSINESS-DATE OF WS-VATW-CA
           MOVE "AR" TO VW-SOURCE-MODULE
           MOVE "CUSTOMERDEPOSIT" TO VW-DOC-TYPE
           MOVE DP-DEPOSIT-REF TO VW-DOC-REF
           MOVE "CUSTOMER" TO VW-PARTY-TYPE
           MOVE DP-CUSTOMER TO VW-PARTY-CODE
           MOVE DP-DOC-DATE TO VW-TAX-POINT
           MOVE DP-POSTING-DATE TO VW-POSTING-DATE
           MOVE DP-VAT-CODE TO VW-VAT-CODE
           MOVE "OUTPUT" TO VW-DIRECTION
           MOVE "GBP" TO VW-CURRENCY
           MOVE 1 TO VW-FX-RATE
           MOVE DP-NET-GBP TO VW-NET
           MOVE DP-NET-GBP TO VW-NET-GBP
           MOVE DP-VAT-GBP TO VW-VAT
           MOVE DP-VAT-GBP TO VW-VAT-GBP
           MOVE PG-JOURNAL-ID TO VW-JOURNAL-ID
           CALL "ARVATWRT" USING WS-VATW-CA
           IF CA-RETURN-CODE OF WS-VATW-CA > 4
               MOVE CA-RETURN-CODE OF WS-VATW-CA TO AR-RC
               MOVE CA-REASON-CODE OF WS-VATW-CA TO AR-REASON
               MOVE CA-MESSAGE OF WS-VATW-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       1500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   APPLY - the deposit becomes part of invoice settlement
       2000-APPLY-DEPOSIT SECTION.
       2000-START.
           IF AR-RC-BAD
               GO TO 2000-EXIT
           END-IF
           IF DP-APPLY-AMOUNT <= 0
               MOVE 8 TO AR-RC
               MOVE "DEP0010" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A positive amount is required to apply"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 2000-EXIT
           END-IF
           IF DP-APPLY-AMOUNT > DP-RESIDUAL
               MOVE 8 TO AR-RC
               MOVE "DEP0011" TO AR-REASON
               MOVE DP-RESIDUAL TO AR-MONEY-UK
               MOVE SPACES TO AR-MSG
               STRING "Only " FUNCTION TRIM(AR-MONEY-UK)
                      " remains on deposit "
                      FUNCTION TRIM(DP-DEPOSIT-REF)
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 2000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT ar.Allocation "
                  "(CompanyCode, CustomerCode, SourceType, "
                  "SourceOpenItemId, TargetOpenItemId, EffectiveDate,"
                  " CurrencyCode, AppliedAmount, AppliedAmountGBP, "
                  "Status, CreatedBy) SELECT ?, ?, 'DEPOSIT', ?, "
                  "t.OpenItemId, ?, ?, ?, ?, 'ACTIVE', ? "
                  "FROM ar.OpenItem t WHERE t.CompanyCode = ? AND "
                  "t.DocumentNumber = ?; "
                  "UPDATE ar.OpenItem SET ResidualAmount = "
                  "ResidualAmount + ?, ResidualAmountGBP = "
                  "ResidualAmountGBP + ? WHERE OpenItemId = ?; "
                  "UPDATE ar.OpenItem SET ResidualAmount = "
                  "ResidualAmount - ?, ResidualAmountGBP = "
                  "ResidualAmountGBP - ?, SettlementStatus = "
                  "CASE WHEN ResidualAmount - ? = 0 THEN 'SETTLED' "
                  "ELSE 'PARTIAL' END WHERE CompanyCode = ? AND "
                  "DocumentNumber = ?; "
                  "SELECT CONVERT(varchar(24), SCOPE_IDENTITY()), ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 18 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE DP-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE DP-OPEN-ITEM-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(13)
           MOVE DP-POSTING-DATE TO SQLIO-PARM-VAL(4)
           MOVE DP-CURRENCY TO SQLIO-PARM-VAL(5)
           MOVE DP-APPLY-AMOUNT TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(6)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(7)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(11)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(12)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(14)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(15)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(16)
           MOVE AR-USERID TO SQLIO-PARM-VAL(8)
           MOVE AR-CO TO SQLIO-PARM-VAL(9)
           MOVE DP-APPLY-TO TO SQLIO-PARM-VAL(10)
           MOVE AR-CO TO SQLIO-PARM-VAL(17)
           MOVE DP-APPLY-TO TO SQLIO-PARM-VAL(18)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 18
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
               GO TO 2000-EXIT
           END-IF
           COMPUTE DP-ALLOC-ID = FUNCTION NUMVAL(WS-NEW-ID)
           COMPUTE DP-RESIDUAL = DP-RESIDUAL - DP-APPLY-AMOUNT
           ADD DP-APPLY-AMOUNT TO DP-APPLIED-TOTAL
           MOVE DP-APPLY-AMOUNT TO AR-MONEY-UK
           MOVE DP-RESIDUAL TO AR-MONEY-ED
           MOVE SPACES TO AR-MSG
           STRING FUNCTION TRIM(AR-MONEY-UK) " of deposit "
                  FUNCTION TRIM(DP-DEPOSIT-REF) " applied to "
                  FUNCTION TRIM(DP-APPLY-TO) "; residual "
                  FUNCTION TRIM(AR-MONEY-ED)
               DELIMITED BY SIZE INTO AR-MSG.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   REFUND - only what has not already been applied
       3000-REFUND-DEPOSIT SECTION.
       3000-START.
           IF AR-RC-BAD
               GO TO 3000-EXIT
           END-IF
           IF DP-REFUND-AMOUNT <= 0
               MOVE 8 TO AR-RC
               MOVE "DEP0020" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A positive refund amount is required"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 3000-EXIT
           END-IF
           IF DP-REFUND-AMOUNT > DP-RESIDUAL
               MOVE 8 TO AR-RC
               MOVE "DEP0021" TO AR-REASON
               MOVE DP-RESIDUAL TO AR-MONEY-UK
               MOVE DP-REFUND-AMOUNT TO AR-MONEY-ED
               MOVE SPACES TO AR-MSG
               STRING "A refund of " FUNCTION TRIM(AR-MONEY-ED)
                      " exceeds the deposit residual of "
                      FUNCTION TRIM(AR-MONEY-UK)
                      " - the balance has already been applied"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 3000-EXIT
           END-IF
           IF DP-REASON = SPACES
               MOVE 8 TO AR-RC
               MOVE "DEP0022" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A refund needs an approved reason"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.OpenItem SET ResidualAmount = "
                  "ResidualAmount + ?, ResidualAmountGBP = "
                  "ResidualAmountGBP + ?, SettlementStatus = "
                  "CASE WHEN ResidualAmount + ? = 0 THEN 'SETTLED' "
                  "ELSE 'PARTIAL' END, ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() WHERE OpenItemId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE DP-REFUND-AMOUNT TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(1)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(2)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(3)
           MOVE AR-USERID TO SQLIO-PARM-VAL(4)
           MOVE DP-OPEN-ITEM-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(5)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 5
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 3000-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           COMPUTE DP-RESIDUAL = DP-RESIDUAL - DP-REFUND-AMOUNT
           ADD DP-REFUND-AMOUNT TO DP-REFUNDED-TOTAL
           MOVE DP-REFUND-AMOUNT TO AR-MONEY-UK
           MOVE SPACES TO AR-MSG
           STRING "Refund of " FUNCTION TRIM(AR-MONEY-UK)
                  " approved against deposit "
                  FUNCTION TRIM(DP-DEPOSIT-REF)
               DELIMITED BY SIZE INTO AR-MSG.
       3000-EXIT.
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
           MOVE "DEP0098" TO AR-REASON
           MOVE SPACES TO AR-MSG
           STRING "ARDEPOS does not support function ["
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
