      *>================================================================
      *> PROGRAM-ID : BADDEBT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : VAT BAD DEBT RELIEF.  RELIEF MAY ONLY BE CLAIMED
      *>              WHEN THE OUTPUT VAT WAS ACCOUNTED FOR AND PAID,
      *>              THE DEBT IS AT LEAST SIX MONTHS OVERDUE FROM THE
      *>              LATER OF THE SUPPLY DATE AND THE DUE DATE, IT
      *>              HAS BEEN WRITTEN OFF IN THE VAT ACCOUNT AND IT
      *>              HAS NOT BEEN SOLD OR FACTORED.  A LATER RECEIPT
      *>              REPAYS THE PROPORTIONATE RELIEF.
      *> CALLED BY  : BAD DEBT REVIEW, VAT RETURN PREPARATION (FIN),
      *>              RCPTALOC WHEN A WRITTEN OFF DEBT IS PAID
      *> INTERFACE  : CALL "BADDEBT" USING WS-BADDEBT-CA
      *>              COPYBOOK BADDEBTCA
      *> CALLS      : SQLIO, PARMGET, PERMCHK, ARVATWRT, ARPOSTGL
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2004-01-05 KMP  CR1702 BAD DEBT RELIEF SIX MONTH RULE
      *> 2019-04-01 KMP  CR2410 MTD - BOX 4 CLAIM, BOX 1 REPAYMENT
      *> 2026-10-05 STERP LINKED TO THE ORIGINAL INVOICE
      *>================================================================
      *> REQ: AR-026
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BADDEBT.
       AUTHOR. K M PATEL.
       DATE-WRITTEN. 05/01/2004.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ARWORK.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARM-CA==.
       COPY PERMCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-PERM-CA==.
       COPY ARVATWRTCA REPLACING ==DFHCOMMAREA== BY ==WS-VATW-CA==.
       COPY ARPOSTGLCA REPLACING ==DFHCOMMAREA== BY ==WS-POSTGL-CA==.
       01  WS-ITEM.
           05  WS-IT-ID                PIC X(24).
           05  WS-IT-CUSTOMER          PIC X(20).
           05  WS-IT-DOCNO             PIC X(30).
           05  WS-IT-DUE               PIC X(10).
           05  WS-IT-TAXPOINT          PIC X(10).
           05  WS-IT-RESIDUAL          PIC X(24).
           05  WS-IT-VAT               PIC X(24).
           05  WS-IT-WRITTEN           PIC X(4).
           05  WS-IT-CLAIMED           PIC X(4).
           05  WS-IT-ORIGINAL          PIC X(24).
       01  WS-REL.
           05  WS-RL-ID                PIC X(24).
           05  WS-RL-CLAIMED           PIC X(24).
           05  WS-RL-REPAID            PIC X(24).
           05  WS-RL-STATUS            PIC X(12).
           05  WS-RL-WRITTEN           PIC X(24).
       01  WS-ID-ROW.
           05  WS-NEW-ID               PIC X(24).
           05  WS-ID2                  PIC X(24).
       01  WS-SW.
           05  WS-TRAN-OPEN            PIC X VALUE "N".
           05  WS-MONTHS               PIC S9(3) VALUE 6.
           05  WS-LATER-DATE           PIC 9(8) VALUE 0.
           05  WS-Y                    PIC 9(4) VALUE 0.
           05  WS-M                    PIC 9(2) VALUE 0.
           05  WS-D                    PIC 9(2) VALUE 0.
       LINKAGE SECTION.
       COPY BADDEBTCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF AR-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0300-READ-INVOICE
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE AR-FUNC
               WHEN "ASSESS"   PERFORM 1000-ASSESS
               WHEN "CLAIM"    PERFORM 1000-ASSESS
                               PERFORM 2000-CLAIM-RELIEF
               WHEN "REPAY"    PERFORM 3000-REPAY-RELIEF
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           PERFORM 9900-SET-RESULT
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE AR-WORK
           INITIALIZE AR-CTL
           INITIALIZE WS-ITEM
           INITIALIZE WS-REL
           MOVE "N" TO WS-TRAN-OPEN
           MOVE CA-FUNCTION OF DFHCOMMAREA TO AR-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO AR-CO
           MOVE CA-SITE OF DFHCOMMAREA TO AR-SITE
           MOVE CA-USER OF DFHCOMMAREA TO AR-USERID
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO AR-BUSDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO AR-CORREL
           MOVE "BADDEBT" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO AR-RC AR-MSG-CNT
           MOVE SPACES TO AR-REASON AR-MSG
           MOVE "AR-026" TO AR-REQ-REF
           MOVE "N" TO BD-ELIGIBLE
           MOVE SPACES TO BD-BLOCK-REASON
           MOVE 0 TO BD-RELIEF-GBP BD-REPAY-GBP
           IF BD-RETURN-END-DATE = 0
               MOVE AR-BUSDATE TO BD-RETURN-END-DATE
           END-IF
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE AR-USERID TO CA-USER OF WS-PARM-CA
           MOVE BD-RETURN-END-DATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "VAT.BADDEBT.MONTHS" TO PM-PARAM-CODE
           MOVE "Y" TO PM-USE-DEFAULT
           MOVE "6" TO PM-DEFAULT-TEXT
           CALL "PARMGET" USING WS-PARM-CA
           IF PM-VALUE-NUM > 0
               MOVE PM-VALUE-NUM TO WS-MONTHS
           END-IF
           MOVE WS-MONTHS TO BD-MONTHS-REQUIRED.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF AR-CO = SPACES OR BD-INVOICE-NO = SPACES
               MOVE "BDR0001" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A company and an invoice number are required"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

       0300-READ-INVOICE SECTION.
       0300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), o.OpenItemId), "
                  "o.CustomerCode, o.DocumentNumber, "
                  "CONVERT(char(8), o.DueDate, 112), "
                  "ISNULL((SELECT CONVERT(char(8), i.TaxPointDate, "
                  "112) FROM ar.SalesInvoice i WHERE i.InvoiceId = "
                  "o.SourceInvoiceId), CONVERT(char(8), "
                  "o.DocumentDate, 112)), "
                  "CONVERT(varchar(24), o.ResidualAmountGBP), "
                  "CONVERT(varchar(24), o.VatAmountGBP), "
                  "CONVERT(char(1), o.WrittenOff), "
                  "CONVERT(char(1), o.BadDebtReliefClaimed), "
                  "CONVERT(varchar(24), o.OriginalAmountGBP) "
                  "FROM ar.OpenItem o WHERE o.CompanyCode = ? "
                  "AND o.DocumentNumber = ? AND o.ItemType = "
                  "'INVOICE'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE BD-INVOICE-NO TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 10 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 30 TO SQLIO-COL-WIDTH(3)
           MOVE 10 TO SQLIO-COL-WIDTH(4)
           MOVE 10 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 4 TO SQLIO-COL-WIDTH(8)
           MOVE 4 TO SQLIO-COL-WIDTH(9)
           MOVE 24 TO SQLIO-COL-WIDTH(10)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ITEM
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "BDR0002" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Invoice " FUNCTION TRIM(BD-INVOICE-NO)
                      " is not held in the open item ledger"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 0300-EXIT
           END-IF
           COMPUTE BD-OPEN-ITEM-ID = FUNCTION NUMVAL(WS-IT-ID)
           MOVE WS-IT-CUSTOMER TO BD-CUSTOMER
           IF WS-IT-DUE(1:8) IS NUMERIC
               MOVE WS-IT-DUE(1:8) TO BD-DUE-DATE
           END-IF
           IF WS-IT-TAXPOINT(1:8) IS NUMERIC
               MOVE WS-IT-TAXPOINT(1:8) TO BD-SUPPLY-DATE
           END-IF
           COMPUTE BD-OUTSTANDING-GBP =
               FUNCTION NUMVAL(WS-IT-RESIDUAL)
           IF BD-VAT-ACCOUNTED = 0
               COMPUTE BD-VAT-ACCOUNTED = FUNCTION NUMVAL(WS-IT-VAT)
           END-IF
           IF WS-IT-WRITTEN(1:1) = "1"
               MOVE "Y" TO BD-WRITTEN-OFF
           END-IF
           IF BD-WRITEOFF-GBP = 0
               MOVE BD-OUTSTANDING-GBP TO BD-WRITEOFF-GBP
           END-IF
           PERFORM 0350-READ-RELIEF.
       0300-EXIT.
           EXIT.

       0350-READ-RELIEF SECTION.
       0350-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), r.ReliefId), "
                  "CONVERT(varchar(24), r.ReliefClaimedGBP), "
                  "CONVERT(varchar(24), r.RepaidGBP), r.Status, "
                  "CONVERT(varchar(24), r.WrittenOffGBP) "
                  "FROM ar.BadDebtRelief r WHERE r.CompanyCode = ? "
                  "AND r.InvoiceNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE BD-INVOICE-NO TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 12 TO SQLIO-COL-WIDTH(4)
           MOVE 24 TO SQLIO-COL-WIDTH(5)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-REL
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0350-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 0 TO BD-RELIEF-ID
               MOVE 0 TO BD-RELIEF-CLAIMED
               MOVE 0 TO BD-REPAID-TOTAL
               MOVE SPACES TO BD-STATUS
               GO TO 0350-EXIT
           END-IF
           COMPUTE BD-RELIEF-ID = FUNCTION NUMVAL(WS-RL-ID)
           COMPUTE BD-RELIEF-CLAIMED = FUNCTION NUMVAL(WS-RL-CLAIMED)
           COMPUTE BD-REPAID-TOTAL = FUNCTION NUMVAL(WS-RL-REPAID)
           MOVE WS-RL-STATUS TO BD-STATUS
           COMPUTE BD-WRITEOFF-GBP = FUNCTION NUMVAL(WS-RL-WRITTEN).
       0350-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   six months from the later of the supply date and the due
      *>   date.  Spec F5: supply and due 31/01/2027 give 31/07/2027.
       1000-ASSESS SECTION.
       1000-START.
           MOVE "Y" TO BD-ELIGIBLE
           MOVE SPACES TO BD-BLOCK-REASON
           IF BD-SUPPLY-DATE > BD-DUE-DATE
               MOVE BD-SUPPLY-DATE TO WS-LATER-DATE
           ELSE
               MOVE BD-DUE-DATE TO WS-LATER-DATE
           END-IF
           PERFORM 1100-ADD-MONTHS
           MOVE 4 TO BD-VAT-BOX
           COMPUTE BD-RELIEF-GBP ROUNDED =
               BD-VAT-ACCOUNTED - BD-RELIEF-CLAIMED
           IF BD-SOLD-FACTORED = "Y"
               MOVE "N" TO BD-ELIGIBLE
               MOVE SPACES TO BD-BLOCK-REASON
               STRING "The debt has been sold or factored, so relief"
                      " cannot be claimed"
                   DELIMITED BY SIZE INTO BD-BLOCK-REASON
           END-IF
           IF BD-WRITTEN-OFF NOT = "Y"
               MOVE "N" TO BD-ELIGIBLE
               MOVE SPACES TO BD-BLOCK-REASON
               STRING "The debt has not been written off in the VAT "
                      "account" DELIMITED BY SIZE INTO BD-BLOCK-REASON
           END-IF
           IF BD-VAT-PAID = "N"
               MOVE "N" TO BD-ELIGIBLE
               MOVE SPACES TO BD-BLOCK-REASON
               STRING "The output VAT on the supply has not been "
                      "accounted for and paid"
                   DELIMITED BY SIZE INTO BD-BLOCK-REASON
           END-IF
           IF BD-RETURN-END-DATE < BD-ELIGIBLE-FROM
               MOVE "N" TO BD-ELIGIBLE
               MOVE SPACES TO BD-BLOCK-REASON
               STRING "The debt is not yet six months overdue; "
                      "relief is first claimable in a return covering"
                      " " BD-ELIGIBLE-FROM(7:2) "/"
                      BD-ELIGIBLE-FROM(5:2) "/"
                      BD-ELIGIBLE-FROM(1:4)
                   DELIMITED BY SIZE INTO BD-BLOCK-REASON
           END-IF
           IF BD-RELIEF-GBP <= 0
               MOVE "N" TO BD-ELIGIBLE
               MOVE SPACES TO BD-BLOCK-REASON
               STRING "There is no unclaimed VAT on this supply"
                   DELIMITED BY SIZE INTO BD-BLOCK-REASON
           END-IF
           IF BD-ELIGIBLE = "N"
               MOVE 8 TO AR-RC
               MOVE "BDR0010" TO AR-REASON
               MOVE BD-BLOCK-REASON TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               MOVE 0 TO BD-RELIEF-GBP
           ELSE
               MOVE BD-RELIEF-GBP TO AR-MONEY-UK
               MOVE SPACES TO AR-MSG
               STRING "Relief of GBP " FUNCTION TRIM(AR-MONEY-UK)
                      " may be reclaimed in Box 4 for a return "
                      "ending on or after " BD-ELIGIBLE-FROM(7:2) "/"
                      BD-ELIGIBLE-FROM(5:2) "/"
                      BD-ELIGIBLE-FROM(1:4)
                   DELIMITED BY SIZE INTO AR-MSG
           END-IF.
       1000-EXIT.
           EXIT.

      *>   add the qualifying months, clamping to the real month end
       1100-ADD-MONTHS SECTION.
       1100-START.
           MOVE WS-LATER-DATE TO AR-NUM-DATE
           MOVE AR-NUM-DATE TO AR-COMPACT-DATE
           MOVE AR-CMP-Y TO WS-Y
           MOVE AR-CMP-M TO WS-M
           MOVE AR-CMP-D TO WS-D
           COMPUTE AR-N = WS-M + WS-MONTHS
           PERFORM UNTIL AR-N <= 12
               COMPUTE AR-N = AR-N - 12
               COMPUTE WS-Y = WS-Y + 1
           END-PERFORM
           MOVE AR-N TO WS-M
           IF WS-M = 2 AND WS-D > 28
               MOVE 28 TO WS-D
           END-IF
           IF (WS-M = 4 OR WS-M = 6 OR WS-M = 9 OR WS-M = 11)
              AND WS-D > 30
               MOVE 30 TO WS-D
           END-IF
           COMPUTE BD-ELIGIBLE-FROM =
               (WS-Y * 10000) + (WS-M * 100) + WS-D.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   DR VAT recoverable, CR bad debt expense, and the Box 4
      *>   digital VAT record linked to the original invoice
       2000-CLAIM-RELIEF SECTION.
       2000-START.
           IF BD-ELIGIBLE NOT = "Y"
               GO TO 2000-EXIT
           END-IF
           PERFORM 2050-PERMISSION
           IF AR-RC-BAD
               GO TO 2000-EXIT
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "Y" TO WS-TRAN-OPEN
           PERFORM 2100-WRITE-RELIEF-ROW
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-POST-RELIEF
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 2000-EXIT
           END-IF
           PERFORM 2300-VAT-RECORD
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 2000-EXIT
           END-IF
           MOVE "COMT" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "N" TO WS-TRAN-OPEN
           MOVE "CLAIMED" TO BD-STATUS
           MOVE BD-RELIEF-GBP TO AR-MONEY-UK
           MOVE SPACES TO AR-MSG
           STRING "Bad debt relief of GBP "
                  FUNCTION TRIM(AR-MONEY-UK)
                  " claimed in Box 4 for period "
                  FUNCTION TRIM(BD-VAT-PERIOD-KEY)
                  ", linked to " FUNCTION TRIM(BD-INVOICE-NO)
               DELIMITED BY SIZE INTO AR-MSG.
       2000-EXIT.
           EXIT.

       2050-PERMISSION SECTION.
       2050-START.
           INITIALIZE WS-PERM-CA
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PERM-CA
           MOVE AR-SITE TO CA-SITE OF WS-PERM-CA
           MOVE AR-USERID TO CA-USER OF WS-PERM-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-PERM-CA
           MOVE "AR.BADDEBT.CLAIM" TO PK-PERMISSION
           CALL "PERMCHK" USING WS-PERM-CA
           IF CA-RETURN-CODE OF WS-PERM-CA > 4
               MOVE 6 TO AR-RC
               MOVE "BDR0011" TO AR-REASON
               MOVE CA-MESSAGE OF WS-PERM-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       2050-EXIT.
           EXIT.

       2100-WRITE-RELIEF-ROW SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; "
                  "DELETE ar.BadDebtRelief WHERE CompanyCode = ? AND "
                  "InvoiceNumber = ?; "
                  "INSERT ar.BadDebtRelief (CompanyCode, "
                  "CustomerCode, OpenItemId, InvoiceNumber, "
                  "SupplyDate, DueDate, EligibleFromDate, "
                  "WrittenOffDate, WrittenOffGBP, VatAccountedGBP, "
                  "ReliefClaimedGBP, ClaimVatPeriodKey, "
                  "IsSoldOrFactored, Status, CreatedBy) VALUES "
                  "(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 0, "
                  "'CLAIMED', ?); "
                  "UPDATE ar.OpenItem SET BadDebtReliefClaimed = 1, "
                  "WrittenOff = 1 WHERE OpenItemId = ?; "
                  "SELECT CONVERT(varchar(24), SCOPE_IDENTITY()), ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 15 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE BD-INVOICE-NO TO SQLIO-PARM-VAL(2)
           MOVE AR-CO TO SQLIO-PARM-VAL(3)
           MOVE BD-CUSTOMER TO SQLIO-PARM-VAL(4)
           MOVE BD-OPEN-ITEM-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(5)
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(15)
           MOVE BD-INVOICE-NO TO SQLIO-PARM-VAL(6)
           MOVE BD-SUPPLY-DATE TO SQLIO-PARM-VAL(7)
           MOVE BD-DUE-DATE TO SQLIO-PARM-VAL(8)
           MOVE BD-ELIGIBLE-FROM TO SQLIO-PARM-VAL(9)
           IF BD-WRITEOFF-DATE = 0
               MOVE BD-RETURN-END-DATE TO SQLIO-PARM-VAL(10)
           ELSE
               MOVE BD-WRITEOFF-DATE TO SQLIO-PARM-VAL(10)
           END-IF
           MOVE BD-WRITEOFF-GBP TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(11)
           MOVE BD-VAT-ACCOUNTED TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(12)
           MOVE BD-RELIEF-GBP TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(13)
           MOVE BD-VAT-PERIOD-KEY TO SQLIO-PARM-VAL(14)
           MOVE AR-USERID TO SQLIO-PARM-VAL(15)
           MOVE BD-OPEN-ITEM-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(16)
           MOVE 16 TO SQLIO-PARM-CNT
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
               GO TO 2100-EXIT
           END-IF
           COMPUTE BD-RELIEF-ID = FUNCTION NUMVAL(WS-NEW-ID).
       2100-EXIT.
           EXIT.

       2200-POST-RELIEF SECTION.
       2200-START.
           INITIALIZE WS-POSTGL-CA
           MOVE "POST" TO CA-FUNCTION OF WS-POSTGL-CA
           MOVE AR-CO TO CA-COMPANY OF WS-POSTGL-CA
           MOVE AR-USERID TO CA-USER OF WS-POSTGL-CA
           MOVE AR-CORREL TO CA-CORRELATION-ID OF WS-POSTGL-CA
           MOVE BD-RETURN-END-DATE TO
               CA-BUSINESS-DATE OF WS-POSTGL-CA
           MOVE "AR.BADDEBTRELIEF" TO PG-EVENT-CODE
           MOVE "AR" TO PG-JOURNAL-TYPE
           MOVE "AR" TO PG-SOURCE-MODULE
           MOVE BD-INVOICE-NO TO PG-SOURCE-REF
           MOVE BD-RETURN-END-DATE TO PG-DOC-DATE
           MOVE BD-RETURN-END-DATE TO PG-POSTING-DATE
           MOVE "GBP" TO PG-CURRENCY
           MOVE 1 TO PG-FX-RATE
           MOVE "N" TO PG-OWN-TRAN
           MOVE SPACES TO PG-DESCRIPTION
           STRING "VAT bad debt relief on "
                  FUNCTION TRIM(BD-INVOICE-NO)
               DELIMITED BY SIZE INTO PG-DESCRIPTION
           MOVE 2 TO PG-LEG-CNT
           MOVE "VATIN" TO PG-G-LEG-CODE(1)
           MOVE BD-RELIEF-GBP TO PG-G-DEBIT(1)
           MOVE "Bad debt relief reclaimed" TO PG-G-NARRATIVE(1)
           MOVE "VATCTL" TO PG-G-LEG-CODE(2)
           MOVE BD-RELIEF-GBP TO PG-G-CREDIT(2)
           MOVE BD-INVOICE-NO TO PG-G-NARRATIVE(2)
           CALL "ARPOSTGL" USING WS-POSTGL-CA
           IF CA-RETURN-CODE OF WS-POSTGL-CA > 4
               MOVE CA-RETURN-CODE OF WS-POSTGL-CA TO AR-RC
               MOVE CA-REASON-CODE OF WS-POSTGL-CA TO AR-REASON
               MOVE CA-MESSAGE OF WS-POSTGL-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 2200-EXIT
           END-IF
           MOVE PG-JOURNAL-NO TO BD-JOURNAL-NO.
       2200-EXIT.
           EXIT.

       2300-VAT-RECORD SECTION.
       2300-START.
           INITIALIZE WS-VATW-CA
           MOVE "WRITE" TO CA-FUNCTION OF WS-VATW-CA
           MOVE AR-CO TO CA-COMPANY OF WS-VATW-CA
           MOVE AR-USERID TO CA-USER OF WS-VATW-CA
           MOVE BD-RETURN-END-DATE TO CA-BUSINESS-DATE OF WS-VATW-CA
           MOVE "AR" TO VW-SOURCE-MODULE
           MOVE "BADDEBTRELIEF" TO VW-DOC-TYPE
           MOVE BD-INVOICE-NO TO VW-DOC-REF
           MOVE "CUSTOMER" TO VW-PARTY-TYPE
           MOVE BD-CUSTOMER TO VW-PARTY-CODE
           MOVE BD-RETURN-END-DATE TO VW-TAX-POINT
           MOVE BD-RETURN-END-DATE TO VW-POSTING-DATE
           MOVE BD-VAT-PERIOD-KEY TO VW-PERIOD-KEY
           MOVE "S" TO VW-VAT-CODE
           MOVE "INPUT" TO VW-DIRECTION
           MOVE "GBP" TO VW-CURRENCY
           MOVE 1 TO VW-FX-RATE
           MOVE 0 TO VW-NET
           MOVE 0 TO VW-NET-GBP
           MOVE BD-RELIEF-GBP TO VW-VAT
           MOVE BD-RELIEF-GBP TO VW-VAT-GBP
           MOVE BD-RELIEF-GBP TO VW-RECOVERABLE-GBP
           MOVE "BADDEBTRELIEF" TO VW-ADJUST-TYPE
           MOVE PG-JOURNAL-ID TO VW-JOURNAL-ID
           CALL "ARVATWRT" USING WS-VATW-CA
           IF CA-RETURN-CODE OF WS-VATW-CA > 4
               MOVE CA-RETURN-CODE OF WS-VATW-CA TO AR-RC
               MOVE CA-REASON-CODE OF WS-VATW-CA TO AR-REASON
               MOVE CA-MESSAGE OF WS-VATW-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 2300-EXIT
           END-IF
           MOVE VW-VAT-BOX TO BD-VAT-BOX
           MOVE 4 TO BD-VAT-BOX.
       2300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   a later receipt repays the proportionate relief through
      *>   Box 1.  Spec F5: 600.00 of a 1,200.00 debt repays 100.00.
       3000-REPAY-RELIEF SECTION.
       3000-START.
           IF BD-RELIEF-ID = 0 OR BD-RELIEF-CLAIMED <= 0
               MOVE 16 TO AR-RC
               MOVE "BDR0020" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "No bad debt relief has been claimed on "
                      FUNCTION TRIM(BD-INVOICE-NO)
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 3000-EXIT
           END-IF
           IF BD-RECEIPT-GBP <= 0
               MOVE 8 TO AR-RC
               MOVE "BDR0021" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A positive receipt is required to repay relief"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 3000-EXIT
           END-IF
           IF BD-WRITEOFF-GBP <= 0
               MOVE 8 TO AR-RC
               MOVE "BDR0022" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The written off amount is not known, so the "
                      "proportion cannot be established"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 3000-EXIT
           END-IF
           COMPUTE AR-AMT1 ROUNDED =
               BD-RELIEF-CLAIMED * BD-RECEIPT-GBP / BD-WRITEOFF-GBP
           COMPUTE AR-AMT2 = BD-RELIEF-CLAIMED - BD-REPAID-TOTAL
           IF AR-AMT1 > AR-AMT2
               MOVE AR-AMT2 TO AR-AMT1
           END-IF
           MOVE AR-AMT1 TO BD-REPAY-GBP
           MOVE 1 TO BD-VAT-BOX
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "Y" TO WS-TRAN-OPEN
           PERFORM 3100-WRITE-REPAYMENT
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 3000-EXIT
           END-IF
           PERFORM 3200-REPAY-VAT-RECORD
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 3000-EXIT
           END-IF
           MOVE "COMT" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "N" TO WS-TRAN-OPEN
           MOVE BD-REPAY-GBP TO AR-MONEY-UK
           MOVE SPACES TO AR-MSG
           STRING "GBP " FUNCTION TRIM(AR-MONEY-UK)
                  " of bad debt relief is repaid through Box 1, "
                  "linked to " FUNCTION TRIM(BD-INVOICE-NO)
               DELIMITED BY SIZE INTO AR-MSG.
       3000-EXIT.
           EXIT.

       3100-WRITE-REPAYMENT SECTION.
       3100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.BadDebtRelief SET RepaidGBP = RepaidGBP "
                  "+ ?, RepaymentVatPeriodKey = ?, Status = "
                  "CASE WHEN RepaidGBP + ? >= ReliefClaimedGBP THEN "
                  "'REPAID' ELSE 'PARTREPAID' END "
                  "WHERE ReliefId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE BD-REPAY-GBP TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(1)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(3)
           IF BD-REPAY-PERIOD = SPACES
               MOVE BD-VAT-PERIOD-KEY TO SQLIO-PARM-VAL(2)
           ELSE
               MOVE BD-REPAY-PERIOD TO SQLIO-PARM-VAL(2)
           END-IF
           MOVE BD-RELIEF-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(4)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 4
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 3100-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW.
       3100-EXIT.
           EXIT.

       3200-REPAY-VAT-RECORD SECTION.
       3200-START.
           INITIALIZE WS-VATW-CA
           MOVE "WRITE" TO CA-FUNCTION OF WS-VATW-CA
           MOVE AR-CO TO CA-COMPANY OF WS-VATW-CA
           MOVE AR-USERID TO CA-USER OF WS-VATW-CA
           MOVE BD-RETURN-END-DATE TO CA-BUSINESS-DATE OF WS-VATW-CA
           MOVE "AR" TO VW-SOURCE-MODULE
           MOVE "BADDEBTREPAY" TO VW-DOC-TYPE
           MOVE BD-INVOICE-NO TO VW-DOC-REF
           MOVE "CUSTOMER" TO VW-PARTY-TYPE
           MOVE BD-CUSTOMER TO VW-PARTY-CODE
           MOVE BD-RETURN-END-DATE TO VW-TAX-POINT
           MOVE BD-RETURN-END-DATE TO VW-POSTING-DATE
           IF BD-REPAY-PERIOD NOT = SPACES
               MOVE BD-REPAY-PERIOD TO VW-PERIOD-KEY
           END-IF
           MOVE "S" TO VW-VAT-CODE
           MOVE "OUTPUT" TO VW-DIRECTION
           MOVE "GBP" TO VW-CURRENCY
           MOVE 1 TO VW-FX-RATE
           MOVE 0 TO VW-NET
           MOVE 0 TO VW-NET-GBP
           MOVE BD-REPAY-GBP TO VW-VAT
           MOVE BD-REPAY-GBP TO VW-VAT-GBP
           MOVE "BADDEBTREPAY" TO VW-ADJUST-TYPE
           CALL "ARVATWRT" USING WS-VATW-CA
           IF CA-RETURN-CODE OF WS-VATW-CA > 4
               MOVE CA-RETURN-CODE OF WS-VATW-CA TO AR-RC
               MOVE CA-REASON-CODE OF WS-VATW-CA TO AR-REASON
               MOVE CA-MESSAGE OF WS-VATW-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
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
           MOVE "BDR0098" TO AR-REASON
           MOVE SPACES TO AR-MSG
           STRING "BADDEBT does not support function ["
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
