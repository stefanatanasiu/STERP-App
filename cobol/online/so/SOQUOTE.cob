      *>================================================================
      *> PROGRAM-ID : SOQUOTE
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT
      *> PURPOSE    : QUOTATIONS AND CONTROLLED NEW DEMAND.  A QUOTE IS
      *>              NOT A SALES COMMITMENT.  EVERY ISSUED VERSION IS
      *>              KEPT, SO WHAT THE CUSTOMER ACCEPTED CAN ALWAYS BE
      *>              REPRODUCED.  CONVERSION CARRIES THE ACCEPTED
      *>              TERMS, PRICES, VAT CODES AND PROVENANCE ONTO THE
      *>              SALES ORDER AND IS IDEMPOTENT: REPEATING IT
      *>              RETURNS THE ORDER ALREADY CREATED, NEVER A
      *>              SECOND DEMAND.  TEMPLATES COPY COMMERCIAL
      *>              CONTENT ONLY - NO FULFILMENT, DESPATCH, BILLING
      *>              OR PAYMENT STATE TRAVELS WITH THE COPY.  A
      *>              CALL-OFF IS BOUNDED BY THE BLANKET'S REMAINING
      *>              APPROVED QUANTITY AND VALUE.
      *> CALLED BY  : QUOTE ENTRY, ORDER ENTRY, .NET CLIENT
      *> INTERFACE  : CALL "SOQUOTE" USING WS-SOQUOTE-CA
      *>              COPYBOOK SOQUOTECA
      *> CALLS      : SQLIO, NUMGEN, IDEMPOT, DATECALC, AUDITLOG
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-07-08 RJH  CR0455 QUOTATION REGISTER (ACUCOBOL)
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT VALIDITY DATES THROUGHOUT
      *> 2004-02-11 RJH  CR1142 BLANKET ORDERS AND CALL-OFFS
      *> 2009-08-26 KMP  CR2088 TEMPLATES MUST NOT COPY PAID DEPOSITS
      *> 2026-10-06 STERP SO-001 CONVERSION IS IDEMPOTENT
      *> 2026-10-06 STERP SO-024 BLANKET REMAINING COMMITMENT LIMIT
      *> 2026-10-07 STERP CR2601 ORDER HEADER FROM CUSTOMER MASTER
      *>================================================================
      *> REQ: SO-001, SO-024, SO-022
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOQUOTE.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 08/07/1996.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY NUMGENCA REPLACING ==DFHCOMMAREA== BY ==WS-NUM-CA==.
       COPY IDEMPOTCA REPLACING ==DFHCOMMAREA== BY ==WS-IDEM-CA==.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DATE-CA==.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUD-CA==.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARM-CA==.
       01  WS-CTL.
           05  WS-FUNC                 PIC X(8).
           05  WS-CO                   PIC X(10).
           05  WS-SITE                 PIC X(10).
           05  WS-USER                 PIC X(20).
           05  WS-BUSDATE              PIC 9(8).
           05  WS-CORREL               PIC X(36).
           05  WS-RC                   PIC 9(2).
           05  WS-REASON               PIC X(8).
           05  WS-MSG                  PIC X(200).
           05  WS-MSGCNT               PIC 9(2).
           05  WS-TRAN-OPEN            PIC X.
           05  WS-IDEM-OPEN            PIC X.
           05  WS-I                    PIC S9(4).
           05  WS-J                    PIC S9(4).
           05  WS-P                    PIC S9(4).
           05  WS-LINES-IN             PIC S9(4).
           05  WS-SERIES               PIC X(20).
           05  WS-NEWNUM               PIC X(30).
           05  WS-SAVE-VER             PIC S9(4).
           05  WS-NEW-STATUS           PIC X(12).
           05  WS-LINE-REF             PIC X(10).
           05  WS-LINE-REF-N           PIC 9(4).
           05  WS-LINE-TYPE            PIC X(10).
           05  WS-ORDER-ID             PIC X(30).
           05  WS-OPID                 PIC X(60).
       01  WS-EDITS.
           05  WS-MONEY-ED             PIC -(13)9.99.
           05  WS-QTY-ED               PIC -(13)9.999999.
           05  WS-PRICE-ED             PIC -(11)9.999999.
           05  WS-RATE-ED              PIC -(9)9.99999999.
           05  WS-PCT-ED               PIC -(5)9.9999.
           05  WS-INT-ED               PIC -(9)9.
           05  WS-UK-MONEY             PIC Z,ZZZ,ZZZ,ZZ9.99-.
           05  WS-UK-QTY               PIC Z,ZZZ,ZZ9.999999-.
           05  WS-DATE-ED              PIC 9(8).
           05  WS-VER-ED               PIC Z(3)9.
       01  WS-AMTS.
           05  WS-AMT1                 PIC S9(13)V99.
           05  WS-AMT2                 PIC S9(13)V99.
           05  WS-AMT3                 PIC S9(13)V99.
           05  WS-PPQ                  PIC S9(9)V9(6).
           05  WS-COST-TOT             PIC S9(13)V99.
      *>   quote header read back from so.Quote
       01  WS-QHDR.
           05  WS-Q-ID                 PIC X(24).
           05  WS-Q-NUMBER             PIC X(30).
           05  WS-Q-VERSION            PIC X(6).
           05  WS-Q-CUSTOMER           PIC X(20).
           05  WS-Q-STATUS             PIC X(12).
           05  WS-Q-DATE                PIC X(8).
           05  WS-Q-VALID              PIC X(8).
           05  WS-Q-CURRENCY           PIC X(3).
           05  WS-Q-FX                 PIC X(24).
           05  WS-Q-TERMS              PIC X(12).
           05  WS-Q-TERMSVER           PIC X(20).
           05  WS-Q-SALESPERSON        PIC X(20).
           05  WS-Q-NET                PIC X(24).
           05  WS-Q-VAT                PIC X(24).
           05  WS-Q-GROSS              PIC X(24).
           05  WS-Q-CONVERTED          PIC X(30).
           05  WS-Q-PROSPECT           PIC X(120).
      *>   one quote or order line as fetched
       01  WS-QLIN.
           05  WS-QL-NUM               PIC X(10).
           05  WS-QL-ITEM              PIC X(40).
           05  WS-QL-REV               PIC X(4).
           05  WS-QL-DESC              PIC X(200).
           05  WS-QL-QTY               PIC X(24).
           05  WS-QL-UOM               PIC X(10).
           05  WS-QL-PRICE             PIC X(24).
           05  WS-QL-PPQ               PIC X(24).
           05  WS-QL-D1                PIC X(12).
           05  WS-QL-D2                PIC X(12).
           05  WS-QL-NET               PIC X(24).
           05  WS-QL-VATCODE           PIC X(4).
           05  WS-QL-VATAMT            PIC X(24).
           05  WS-QL-COST              PIC X(24).
           05  WS-QL-BASIS             PIC X(20).
           05  WS-QL-SOURCE            PIC X(20).
      *>   commercial header used to build a new sales order
       01  WS-ORD.
           05  WS-O-TYPE               PIC X(12).
           05  WS-O-QUOTE-REF          PIC X(30).
           05  WS-O-BLANKET            PIC X(30).
           05  WS-O-CUSTOMER           PIC X(20).
           05  WS-O-INV-CUST           PIC X(20).
           05  WS-O-CURRENCY           PIC X(3).
           05  WS-O-FX                 PIC X(24).
           05  WS-O-TERMS              PIC X(12).
           05  WS-O-TERMS-SNAP         PIC X(100).
           05  WS-O-SALESPERSON        PIC X(20).
           05  WS-O-TC-VERSION         PIC X(20).
           05  WS-O-INCO               PIC X(3).
           05  WS-O-INCO-PLACE         PIC X(60).
           05  WS-O-ADDR-REF           PIC X(10).
           05  WS-O-ADDR-SNAP          PIC X(300).
           05  WS-O-BILL-SNAP          PIC X(300).
           05  WS-O-VAT-BASIS          PIC X(20).
           05  WS-O-VAT-REG            PIC X(15).
           05  WS-O-VAT-LOC            PIC X(3).
           05  WS-O-SITE               PIC X(10).
           05  WS-O-ROT                PIC X(1).
      *>   the account as the sales ledger holds it (CR2601)
       01  WS-CUST.
           05  WS-CU-INV-CUST          PIC X(20).
           05  WS-CU-TERMS             PIC X(12).
           05  WS-CU-VAT-LOC           PIC X(3).
           05  WS-CU-VAT-REG           PIC X(15).
           05  WS-CU-SALESPERSON       PIC X(20).
           05  WS-CU-DELIV-TERMS       PIC X(10).
       01  WS-ADR.
           05  WS-AD-REF               PIC X(10).
           05  WS-AD-SNAP              PIC X(300).
       01  WS-DEF-WORK.
           05  WS-CUST-FOUND           PIC X VALUE "N".
           05  WS-ADDR-ROLE            PIC X(12) VALUE SPACES.
      *>   small general purpose landing areas
       01  WS-BLK.
           05  WS-B-STATUS             PIC X(12).
           05  WS-B-CUSTOMER           PIC X(20).
           05  WS-B-CURRENCY           PIC X(3).
           05  WS-B-QTY                PIC X(24).
           05  WS-B-VALUE              PIC X(24).
       01  WS-TWO.
           05  WS-T1                   PIC X(30).
           05  WS-T2                   PIC X(30).
       01  WS-FOUR.
           05  WS-F1                   PIC X(24).
           05  WS-F2                   PIC X(24).
           05  WS-F3                   PIC X(24).
           05  WS-F4                   PIC X(24).
       LINKAGE SECTION.
       COPY SOQUOTECA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE-INPUT
           IF WS-RC > 4
               GO TO 0000-EXIT
           END-IF
           EVALUATE WS-FUNC
               WHEN "CREATE"   PERFORM 1000-CREATE-QUOTE
               WHEN "REVISE"   PERFORM 1100-REVISE-QUOTE
               WHEN "ISSUE"    PERFORM 1200-ISSUE-QUOTE
               WHEN "ACCEPT"   PERFORM 1300-ACCEPT-QUOTE
               WHEN "DECLINE"  PERFORM 1400-DECLINE-QUOTE
               WHEN "EXPIRE"   PERFORM 1500-EXPIRE-QUOTE
               WHEN "CONVERT"  PERFORM 2000-CONVERT-QUOTE
               WHEN "TEMPLATE" PERFORM 3000-TEMPLATE-COPY
               WHEN "CALLOFF"  PERFORM 4000-BLANKET-CALLOFF
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE
           IF WS-RC > 4
               PERFORM 9500-ROLLBACK
           END-IF.
       0000-EXIT.
           PERFORM 9900-SET-RESULT
           GOBACK.

      *>----------------------------------------------------------------
      *>   WORKING-STORAGE survives between calls under the run time,
      *>   so everything that decides an answer is re-initialised here.
       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE WS-CTL
           INITIALIZE WS-AMTS
           INITIALIZE WS-QHDR
           INITIALIZE WS-QLIN
           INITIALIZE WS-ORD
           INITIALIZE WS-TWO
           INITIALIZE WS-FOUR
           MOVE "N" TO WS-TRAN-OPEN WS-IDEM-OPEN
           MOVE 0 TO WS-RC WS-MSGCNT
           MOVE SPACES TO WS-REASON WS-MSG
           MOVE CA-FUNCTION OF DFHCOMMAREA TO WS-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO WS-CO
           MOVE CA-SITE OF DFHCOMMAREA TO WS-SITE
           MOVE CA-USER OF DFHCOMMAREA TO WS-USER
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO WS-BUSDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO WS-CORREL
           MOVE "SOQUOTE" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE "SO-001" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE SQ-LINE-CNT TO WS-LINES-IN
           MOVE "N" TO SQ-DUP-REF-FOUND
           MOVE "N" TO SQ-ALREADY-DONE
           MOVE SPACES TO SQ-DUP-ORDER-NUMBER
           MOVE 0 TO SQ-LINES-CREATED
           IF SQ-QUOTE-DATE = 0
               MOVE WS-BUSDATE TO SQ-QUOTE-DATE
           END-IF
           IF SQ-CURRENCY = SPACES
               MOVE "GBP" TO SQ-CURRENCY
           END-IF
           IF SQ-FX-RATE = 0
               MOVE 1 TO SQ-FX-RATE
           END-IF
           IF SQ-SITE-CODE = SPACES
               MOVE WS-SITE TO SQ-SITE-CODE
           END-IF
           IF SQ-SOURCE-TYPE = SPACES
               MOVE "ORDER" TO SQ-SOURCE-TYPE
           END-IF
           IF SQ-VALID-DAYS = 0
               MOVE 30 TO SQ-VALID-DAYS
           END-IF
           IF SQ-VALID-UNTIL = 0
               PERFORM 0150-DEFAULT-VALIDITY
           END-IF.
       0100-EXIT.
           EXIT.

      *>   a quote with no stated validity would be an open offer -
      *>   DATECALC gives it the standard thirty calendar days
       0150-DEFAULT-VALIDITY SECTION.
       0150-START.
           INITIALIZE WS-DATE-CA
           MOVE "ADDDAYS" TO CA-FUNCTION OF WS-DATE-CA
           MOVE WS-CO TO CA-COMPANY OF WS-DATE-CA
           MOVE WS-USER TO CA-USER OF WS-DATE-CA
           MOVE WS-BUSDATE TO CA-BUSINESS-DATE OF WS-DATE-CA
           MOVE SQ-QUOTE-DATE TO DC-DATE-1
           MOVE SQ-VALID-DAYS TO DC-NUMBER
           CALL "DATECALC" USING WS-DATE-CA
           IF CA-RETURN-CODE OF WS-DATE-CA < 8 AND DC-RESULT-DATE > 0
               MOVE DC-RESULT-DATE TO SQ-VALID-UNTIL
           ELSE
               MOVE SQ-QUOTE-DATE TO SQ-VALID-UNTIL
           END-IF.
       0150-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0200-VALIDATE-INPUT SECTION.
       0200-START.
           IF WS-CO = SPACES
               MOVE "SOQ0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A company code is required"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF WS-FUNC = "CREATE"
               IF SQ-CUSTOMER = SPACES AND SQ-PROSPECT-NAME = SPACES
                   MOVE "SOQ0001" TO WS-REASON
                   MOVE SPACES TO WS-MSG
                   STRING "A quote needs a customer or a prospect name"
                       DELIMITED BY SIZE INTO WS-MSG
                   PERFORM 9000-REJECT
                   GO TO 0200-EXIT
               END-IF
               IF SQ-LINE-CNT = 0
                   MOVE "SOQ0013" TO WS-REASON
                   MOVE SPACES TO WS-MSG
                   STRING "At least one quote line is required"
                       DELIMITED BY SIZE INTO WS-MSG
                   PERFORM 9000-REJECT
                   GO TO 0200-EXIT
               END-IF
           END-IF
           IF SQ-LINE-CNT > 40
               MOVE 40 TO SQ-LINE-CNT
               MOVE 40 TO WS-LINES-IN
           END-IF
           IF WS-FUNC = "REVISE" OR WS-FUNC = "ISSUE" OR
              WS-FUNC = "ACCEPT" OR WS-FUNC = "DECLINE" OR
              WS-FUNC = "EXPIRE" OR WS-FUNC = "CONVERT"
               IF SQ-QUOTE-NUMBER = SPACES
                   MOVE "SOQ0001" TO WS-REASON
                   MOVE SPACES TO WS-MSG
                   STRING "A quote number is required"
                       DELIMITED BY SIZE INTO WS-MSG
                   PERFORM 9000-REJECT
                   GO TO 0200-EXIT
               END-IF
           END-IF
           IF WS-FUNC = "TEMPLATE" AND SQ-SOURCE-REF = SPACES
               MOVE "SOQ0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A source order or quote reference is required"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF WS-FUNC = "CALLOFF" AND SQ-BLANKET-NUMBER = SPACES
               MOVE "SOQ0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A blanket order number is required"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   version 0 means "the one that matters": the accepted version
      *>   for CONVERT, otherwise the highest version held.  A version
      *>   that has already been converted still counts for CONVERT -
      *>   the repeat has to find it to be told about the order it
      *>   already produced rather than be refused (COM-015).
       0300-RESOLVE-VERSION SECTION.
       0300-START.
           IF SQ-VERSION NOT = 0
               GO TO 0300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           IF WS-FUNC = "CONVERT"
               STRING "SELECT CONVERT(varchar(10), ISNULL((SELECT "
                      "TOP (1) q.Version FROM so.Quote q WHERE "
                      "q.CompanyCode = ? AND q.QuoteNumber = ? AND "
                      "(q.Status = 'ACCEPTED' OR "
                      "(q.ConvertedOrderNumber IS NOT NULL AND "
                      "q.Status IN ('CONVERTED','PARTCONV'))) ORDER "
                      "BY CASE WHEN q.Status = 'ACCEPTED' THEN 0 "
                      "ELSE 1 END, q.Version DESC), 0)), ''"
                   DELIMITED BY SIZE INTO SQLIO-STMT
           ELSE
               STRING "SELECT CONVERT(varchar(10), ISNULL(MAX("
                      "Version),0)), '' FROM so.Quote WHERE "
                      "CompanyCode = ? AND QuoteNumber = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
           END-IF
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SQ-QUOTE-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 0 TO SQ-VERSION
           ELSE
               COMPUTE SQ-VERSION = FUNCTION NUMVAL(WS-T1)
           END-IF
           IF SQ-VERSION = 0 AND WS-FUNC = "CONVERT"
               MOVE "SOQ0007" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Quote " FUNCTION TRIM(SQ-QUOTE-NUMBER)
                      " has no accepted version to convert"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
           END-IF.
       0300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0310-READ-QUOTE SECTION.
       0310-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), q.QuoteId), "
                  "q.QuoteNumber, CONVERT(varchar(6), q.Version), "
                  "ISNULL(q.CustomerCode,''), q.Status, "
                  "CONVERT(char(8), q.QuoteDate, 112), "
                  "CONVERT(char(8), q.ValidUntil, 112), "
                  "q.CurrencyCode, CONVERT(varchar(24), q.FxRate), "
                  "ISNULL(q.TermsCode,''), ISNULL(q.TermsVersion,''),"
                  " ISNULL(q.SalespersonCode,''), "
                  "CONVERT(varchar(24), q.NetAmount), "
                  "CONVERT(varchar(24), q.VatAmount), "
                  "CONVERT(varchar(24), q.GrossAmount), "
                  "ISNULL(q.ConvertedOrderNumber,''), "
                  "ISNULL(q.ProspectName,'') FROM so.Quote q WHERE "
                  "q.CompanyCode = ? AND q.QuoteNumber = ? AND "
                  "q.Version = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SQ-QUOTE-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE SQ-VERSION TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           MOVE 17 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 6 TO SQLIO-COL-WIDTH(3)
           MOVE 20 TO SQLIO-COL-WIDTH(4)
           MOVE 12 TO SQLIO-COL-WIDTH(5)
           MOVE 8 TO SQLIO-COL-WIDTH(6)
           MOVE 8 TO SQLIO-COL-WIDTH(7)
           MOVE 3 TO SQLIO-COL-WIDTH(8)
           MOVE 24 TO SQLIO-COL-WIDTH(9)
           MOVE 12 TO SQLIO-COL-WIDTH(10)
           MOVE 20 TO SQLIO-COL-WIDTH(11)
           MOVE 20 TO SQLIO-COL-WIDTH(12)
           MOVE 24 TO SQLIO-COL-WIDTH(13)
           MOVE 24 TO SQLIO-COL-WIDTH(14)
           MOVE 24 TO SQLIO-COL-WIDTH(15)
           MOVE 30 TO SQLIO-COL-WIDTH(16)
           MOVE 120 TO SQLIO-COL-WIDTH(17)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-QHDR
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0310-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO WS-RC
               MOVE "SOQ0002" TO WS-REASON
               MOVE SQ-VERSION TO WS-VER-ED
               MOVE SPACES TO WS-MSG
               STRING "Quote " FUNCTION TRIM(SQ-QUOTE-NUMBER)
                      " version " FUNCTION TRIM(WS-VER-ED)
                      " is not held" DELIMITED BY SIZE INTO WS-MSG
               ADD 1 TO WS-MSGCNT
               GO TO 0310-EXIT
           END-IF
           COMPUTE SQ-QUOTE-ID = FUNCTION NUMVAL(WS-Q-ID)
           MOVE WS-Q-STATUS TO SQ-STATUS
           MOVE WS-Q-CUSTOMER TO SQ-CUSTOMER
           MOVE WS-Q-PROSPECT TO SQ-PROSPECT-NAME
           MOVE WS-Q-CURRENCY TO SQ-CURRENCY
           MOVE WS-Q-TERMS TO SQ-TERMS-CODE
           MOVE WS-Q-TERMSVER TO SQ-TERMS-VERSION
           MOVE WS-Q-SALESPERSON TO SQ-SALESPERSON
           COMPUTE SQ-FX-RATE = FUNCTION NUMVAL(WS-Q-FX)
           COMPUTE SQ-QUOTE-DATE = FUNCTION NUMVAL(WS-Q-DATE)
           COMPUTE SQ-VALID-UNTIL = FUNCTION NUMVAL(WS-Q-VALID)
           COMPUTE SQ-NET-AMOUNT = FUNCTION NUMVAL(WS-Q-NET)
           COMPUTE SQ-VAT-AMOUNT = FUNCTION NUMVAL(WS-Q-VAT)
           COMPUTE SQ-GROSS-AMOUNT = FUNCTION NUMVAL(WS-Q-GROSS)
           MOVE WS-Q-CONVERTED TO SQ-ORDER-NUMBER.
       0310-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   load so.QuoteLine into the COMMAREA line table
       0400-READ-QUOTE-LINES SECTION.
       0400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(10), l.LineNum), "
                  "ISNULL(l.ItemCode,''), ISNULL(l.Revision,''), "
                  "l.Description, CONVERT(varchar(24), l.Quantity), "
                  "ISNULL(l.UomCode,''), CONVERT(varchar(24), "
                  "l.UnitPrice), CONVERT(varchar(24), l.PricePerQty),"
                  " CONVERT(varchar(12), l.DiscountPct1), "
                  "CONVERT(varchar(12), l.DiscountPct2), "
                  "CONVERT(varchar(24), l.NetAmount), "
                  "ISNULL(l.VatCode,''), CONVERT(varchar(24), "
                  "l.VatAmount), CONVERT(varchar(24), "
                  "ISNULL(l.EstimatedCostGBP,0)), "
                  "ISNULL(l.CostBasis,''), 'QUOTE' FROM so.QuoteLine "
                  "l WHERE l.QuoteId = ? ORDER BY l.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE SQ-QUOTE-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           PERFORM 0450-SET-LINE-COLS
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-QLIN
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0400-EXIT
           END-IF
           PERFORM 0460-FETCH-LINE-LOOP
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-QLIN.
       0400-EXIT.
           EXIT.

       0450-SET-LINE-COLS SECTION.
       0450-START.
           MOVE 16 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 4 TO SQLIO-COL-WIDTH(3)
           MOVE 200 TO SQLIO-COL-WIDTH(4)
           MOVE 24 TO SQLIO-COL-WIDTH(5)
           MOVE 10 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 24 TO SQLIO-COL-WIDTH(8)
           MOVE 12 TO SQLIO-COL-WIDTH(9)
           MOVE 12 TO SQLIO-COL-WIDTH(10)
           MOVE 24 TO SQLIO-COL-WIDTH(11)
           MOVE 4 TO SQLIO-COL-WIDTH(12)
           MOVE 24 TO SQLIO-COL-WIDTH(13)
           MOVE 24 TO SQLIO-COL-WIDTH(14)
           MOVE 20 TO SQLIO-COL-WIDTH(15)
           MOVE 20 TO SQLIO-COL-WIDTH(16).
       0450-EXIT.
           EXIT.

       0460-FETCH-LINE-LOOP SECTION.
       0460-START.
           MOVE 0 TO SQ-LINE-CNT
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-QLIN
           PERFORM UNTIL SQLIO-SQLCODE NOT = 0 OR SQ-LINE-CNT >= 40
               ADD 1 TO SQ-LINE-CNT
               MOVE SQ-LINE-CNT TO WS-I
               COMPUTE SQ-L-LINE-NUM(WS-I) =
                       FUNCTION NUMVAL(WS-QL-NUM)
               MOVE WS-QL-ITEM TO SQ-L-ITEM(WS-I)
               MOVE WS-QL-REV TO SQ-L-REVISION(WS-I)
               MOVE WS-QL-DESC TO SQ-L-DESC(WS-I)
               COMPUTE SQ-L-QTY(WS-I) = FUNCTION NUMVAL(WS-QL-QTY)
               MOVE WS-QL-UOM TO SQ-L-UOM(WS-I)
               COMPUTE SQ-L-UNIT-PRICE(WS-I) =
                       FUNCTION NUMVAL(WS-QL-PRICE)
               COMPUTE SQ-L-PRICE-PER(WS-I) =
                       FUNCTION NUMVAL(WS-QL-PPQ)
               COMPUTE SQ-L-DISC1(WS-I) = FUNCTION NUMVAL(WS-QL-D1)
               COMPUTE SQ-L-DISC2(WS-I) = FUNCTION NUMVAL(WS-QL-D2)
               COMPUTE SQ-L-NET(WS-I) = FUNCTION NUMVAL(WS-QL-NET)
               MOVE WS-QL-VATCODE TO SQ-L-VAT-CODE(WS-I)
               COMPUTE SQ-L-VAT-AMT(WS-I) =
                       FUNCTION NUMVAL(WS-QL-VATAMT)
               COMPUTE SQ-L-COST-GBP(WS-I) =
                       FUNCTION NUMVAL(WS-QL-COST)
               MOVE WS-QL-BASIS TO SQ-L-COST-BASIS(WS-I)
               MOVE WS-QL-SOURCE TO SQ-L-PRICE-SRC(WS-I)
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-QLIN
           END-PERFORM.
       0460-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   SO-024 - a repeated customer order reference is a review
      *>   warning, never an automatic refusal and never a duplicate
       0500-DUPLICATE-REF-CHECK SECTION.
       0500-START.
           IF SQ-CUSTOMER-REF = SPACES OR SQ-CUSTOMER = SPACES
               GO TO 0500-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) OrderNumber, '' FROM so.SalesOrder "
                  "WHERE CompanyCode = ? AND CustomerCode = ? AND "
                  "CustomerOrderRef = ? AND CommercialStatus <> "
                  "'CANCELLED' ORDER BY OrderDate DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SQ-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE SQ-CUSTOMER-REF TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               GO TO 0500-EXIT
           END-IF
           MOVE "Y" TO SQ-DUP-REF-FOUND
           MOVE WS-T1 TO SQ-DUP-ORDER-NUMBER
           IF WS-RC < 4
               MOVE 4 TO WS-RC
           END-IF
           MOVE "SOQ0004" TO WS-REASON
           MOVE SPACES TO WS-MSG
           STRING "Customer order reference "
                  FUNCTION TRIM(SQ-CUSTOMER-REF)
                  " is already used on order " FUNCTION TRIM(WS-T1)
                  " - please review before despatch"
               DELIMITED BY SIZE INTO WS-MSG
           ADD 1 TO WS-MSGCNT.
       0500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   CREATE - a new quotation at version 1, status DRAFT
       1000-CREATE-QUOTE SECTION.
       1000-START.
           PERFORM 6400-TOTAL-THE-LINES
           IF SQ-QUOTE-NUMBER = SPACES
               MOVE "SQUO"    TO WS-SERIES
               PERFORM 6000-NEXT-NUMBER
               IF WS-RC > 4
                   GO TO 1000-EXIT
               END-IF
               MOVE WS-NEWNUM TO SQ-QUOTE-NUMBER
           END-IF
           MOVE 1 TO SQ-VERSION
           MOVE "DRAFT" TO SQ-STATUS
           PERFORM 8000-BEGIN-TRAN
           PERFORM 6100-INSERT-QUOTE-HDR
           IF WS-RC > 4
               GO TO 1000-EXIT
           END-IF
           PERFORM 6150-READ-QUOTE-ID
           PERFORM 6200-INSERT-QUOTE-LINES
           IF WS-RC > 4
               GO TO 1000-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           MOVE SPACES TO WS-MSG
           STRING "Quote " FUNCTION TRIM(SQ-QUOTE-NUMBER)
                  " version 1 created as a draft"
               DELIMITED BY SIZE INTO WS-MSG.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   REVISE - SO-022.  A revision is a NEW version; the version
      *>   it replaces becomes REVISED and stays readable for ever.
       1100-REVISE-QUOTE SECTION.
       1100-START.
           PERFORM 0300-RESOLVE-VERSION
           IF WS-RC > 4
               GO TO 1100-EXIT
           END-IF
           PERFORM 0310-READ-QUOTE
           IF WS-RC > 4
               GO TO 1100-EXIT
           END-IF
           IF SQ-STATUS = "CONVERTED"
               MOVE "SOQ0005" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A converted quote cannot be revised - amend "
                      "the resulting order instead"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 1100-EXIT
           END-IF
           MOVE SQ-STATUS TO SQ-PREV-STATUS
           MOVE SQ-VERSION TO WS-SAVE-VER
           IF WS-LINES-IN = 0
               PERFORM 0400-READ-QUOTE-LINES
               IF WS-RC > 4
                   GO TO 1100-EXIT
               END-IF
           END-IF
           PERFORM 6400-TOTAL-THE-LINES
           COMPUTE SQ-VERSION = WS-SAVE-VER + 1
           MOVE "DRAFT" TO SQ-STATUS
           PERFORM 8000-BEGIN-TRAN
           PERFORM 6100-INSERT-QUOTE-HDR
           IF WS-RC > 4
               GO TO 1100-EXIT
           END-IF
           PERFORM 6150-READ-QUOTE-ID
           PERFORM 6200-INSERT-QUOTE-LINES
           IF WS-RC > 4
               GO TO 1100-EXIT
           END-IF
           MOVE "REVISED" TO WS-NEW-STATUS
           MOVE WS-SAVE-VER TO WS-J
           PERFORM 6300-SET-VERSION-STATUS
           IF WS-RC > 4
               GO TO 1100-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           MOVE SQ-VERSION TO WS-VER-ED
           MOVE SPACES TO WS-MSG
           STRING "Quote " FUNCTION TRIM(SQ-QUOTE-NUMBER)
                  " revised to version " FUNCTION TRIM(WS-VER-ED)
                  "; the previous version is retained as REVISED"
               DELIMITED BY SIZE INTO WS-MSG.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1200-ISSUE-QUOTE SECTION.
       1200-START.
           PERFORM 0300-RESOLVE-VERSION
           IF WS-RC > 4
               GO TO 1200-EXIT
           END-IF
           PERFORM 0310-READ-QUOTE
           IF WS-RC > 4
               GO TO 1200-EXIT
           END-IF
           IF SQ-STATUS NOT = "DRAFT"
               PERFORM 9200-BAD-STATUS
               GO TO 1200-EXIT
           END-IF
           MOVE SQ-STATUS TO SQ-PREV-STATUS
           MOVE "ISSUED" TO WS-NEW-STATUS
           MOVE SQ-VERSION TO WS-J
           PERFORM 8000-BEGIN-TRAN
           PERFORM 6300-SET-VERSION-STATUS
           IF WS-RC > 4
               GO TO 1200-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           MOVE "ISSUED" TO SQ-STATUS
           MOVE SPACES TO WS-MSG
           STRING "Quote " FUNCTION TRIM(SQ-QUOTE-NUMBER)
                  " issued to the customer"
               DELIMITED BY SIZE INTO WS-MSG.
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   ACCEPT - the acceptance evidence is the reason the order
      *>   that follows can be defended, so it is mandatory
       1300-ACCEPT-QUOTE SECTION.
       1300-START.
           PERFORM 0300-RESOLVE-VERSION
           IF WS-RC > 4
               GO TO 1300-EXIT
           END-IF
           PERFORM 0310-READ-QUOTE
           IF WS-RC > 4
               GO TO 1300-EXIT
           END-IF
           IF SQ-STATUS NOT = "ISSUED"
               PERFORM 9200-BAD-STATUS
               GO TO 1300-EXIT
           END-IF
           IF SQ-ACCEPT-EVIDENCE = SPACES
               MOVE "SOQ0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Acceptance evidence is required - record how "
                      "the customer accepted this version"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 1300-EXIT
           END-IF
           IF SQ-VALID-UNTIL < WS-BUSDATE
               MOVE "SOQ0006" TO WS-REASON
               MOVE SQ-VALID-UNTIL TO WS-DATE-ED
               MOVE SPACES TO WS-MSG
               STRING "Quote " FUNCTION TRIM(SQ-QUOTE-NUMBER)
                      " expired on " WS-DATE-ED
                      " and cannot be accepted - revise it first"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 1300-EXIT
           END-IF
           MOVE SQ-STATUS TO SQ-PREV-STATUS
           PERFORM 8000-BEGIN-TRAN
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.Quote SET Status = 'ACCEPTED', "
                  "AcceptedAt = SYSDATETIME(), AcceptanceEvidence = "
                  "?, ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE CompanyCode = ? AND QuoteNumber = ? AND "
                  "Version = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE SQ-ACCEPT-EVIDENCE TO SQLIO-PARM-VAL(1)
           MOVE WS-USER TO SQLIO-PARM-VAL(2)
           MOVE WS-CO TO SQLIO-PARM-VAL(3)
           MOVE SQ-QUOTE-NUMBER TO SQLIO-PARM-VAL(4)
           MOVE SQ-VERSION TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS
           IF WS-RC > 4
               GO TO 1300-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           MOVE "ACCEPTED" TO SQ-STATUS
           PERFORM 7500-AUDIT-THE-CHANGE
           MOVE SPACES TO WS-MSG
           STRING "Quote " FUNCTION TRIM(SQ-QUOTE-NUMBER)
                  " accepted on the evidence of "
                  FUNCTION TRIM(SQ-ACCEPT-EVIDENCE)
               DELIMITED BY SIZE INTO WS-MSG.
       1300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1400-DECLINE-QUOTE SECTION.
       1400-START.
           PERFORM 0300-RESOLVE-VERSION
           IF WS-RC > 4
               GO TO 1400-EXIT
           END-IF
           PERFORM 0310-READ-QUOTE
           IF WS-RC > 4
               GO TO 1400-EXIT
           END-IF
           IF SQ-STATUS NOT = "ISSUED"
               PERFORM 9200-BAD-STATUS
               GO TO 1400-EXIT
           END-IF
           IF SQ-DECLINE-REASON = SPACES
               MOVE "SOQ0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A declining customer's reason is required"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 1400-EXIT
           END-IF
           MOVE SQ-STATUS TO SQ-PREV-STATUS
           MOVE "DECLINED" TO WS-NEW-STATUS
           MOVE SQ-VERSION TO WS-J
           PERFORM 8000-BEGIN-TRAN
           PERFORM 6300-SET-VERSION-STATUS
           IF WS-RC > 4
               GO TO 1400-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           MOVE "DECLINED" TO SQ-STATUS
           PERFORM 7500-AUDIT-THE-CHANGE
           MOVE SPACES TO WS-MSG
           STRING "Quote " FUNCTION TRIM(SQ-QUOTE-NUMBER)
                  " declined - " FUNCTION TRIM(SQ-DECLINE-REASON)
               DELIMITED BY SIZE INTO WS-MSG.
       1400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   EXPIRE - only once the validity date has actually passed
       1500-EXPIRE-QUOTE SECTION.
       1500-START.
           PERFORM 0300-RESOLVE-VERSION
           IF WS-RC > 4
               GO TO 1500-EXIT
           END-IF
           PERFORM 0310-READ-QUOTE
           IF WS-RC > 4
               GO TO 1500-EXIT
           END-IF
           IF SQ-STATUS NOT = "ISSUED" AND SQ-STATUS NOT = "DRAFT"
               PERFORM 9200-BAD-STATUS
               GO TO 1500-EXIT
           END-IF
           IF SQ-VALID-UNTIL >= WS-BUSDATE
               MOVE "SOQ0006" TO WS-REASON
               MOVE SQ-VALID-UNTIL TO WS-DATE-ED
               MOVE SPACES TO WS-MSG
               STRING "Quote " FUNCTION TRIM(SQ-QUOTE-NUMBER)
                      " is valid until " WS-DATE-ED
                      " and cannot be expired yet"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 1500-EXIT
           END-IF
           MOVE SQ-STATUS TO SQ-PREV-STATUS
           MOVE "EXPIRED" TO WS-NEW-STATUS
           MOVE SQ-VERSION TO WS-J
           PERFORM 8000-BEGIN-TRAN
           PERFORM 6300-SET-VERSION-STATUS
           IF WS-RC > 4
               GO TO 1500-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           MOVE "EXPIRED" TO SQ-STATUS
           MOVE SPACES TO WS-MSG
           STRING "Quote " FUNCTION TRIM(SQ-QUOTE-NUMBER)
                  " has passed its validity date and is now expired"
               DELIMITED BY SIZE INTO WS-MSG.
       1500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   CONVERT - SO-001.  The accepted version becomes a sales
      *>   order that retains its terms, prices, VAT codes and the
      *>   provenance so.SalesOrder.QuoteNumber.  Two guards make the
      *>   instruction idempotent: so.Quote.ConvertedOrderNumber and
      *>   the IDEMPOT claim on CA-CORRELATION-ID (COM-015).
       2000-CONVERT-QUOTE SECTION.
       2000-START.
           PERFORM 0300-RESOLVE-VERSION
           IF WS-RC > 4
               GO TO 2000-EXIT
           END-IF
           PERFORM 0310-READ-QUOTE
           IF WS-RC > 4
               GO TO 2000-EXIT
           END-IF
           IF SQ-ORDER-NUMBER NOT = SPACES
               PERFORM 9400-ALREADY-CONVERTED
               GO TO 2000-EXIT
           END-IF
           IF SQ-STATUS NOT = "ACCEPTED"
               MOVE "SOQ0007" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Only an accepted quote version can become an "
                      "order; this version is "
                      FUNCTION TRIM(SQ-STATUS)
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           PERFORM 6500-CLAIM-IDEMPOTENCY
           IF WS-RC NOT = 0
               GO TO 2000-EXIT
           END-IF
           PERFORM 0400-READ-QUOTE-LINES
           IF WS-RC > 4 OR SQ-LINE-CNT = 0
               GO TO 2000-EXIT
           END-IF
           PERFORM 0500-DUPLICATE-REF-CHECK
           PERFORM 6600-ORDER-FROM-QUOTE
           MOVE "SORD"    TO WS-SERIES
           PERFORM 6000-NEXT-NUMBER
           IF WS-RC > 4
               GO TO 2000-EXIT
           END-IF
           MOVE WS-NEWNUM TO SQ-ORDER-NUMBER
           PERFORM 8000-BEGIN-TRAN
           PERFORM 7000-INSERT-ORDER-HDR
           IF WS-RC > 4
               GO TO 2000-EXIT
           END-IF
           PERFORM 7100-INSERT-ORDER-LINES
           IF WS-RC > 4
               GO TO 2000-EXIT
           END-IF
           PERFORM 7200-MARK-CONVERTED
           IF WS-RC > 4
               GO TO 2000-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           PERFORM 6550-END-IDEMPOTENCY
           PERFORM 7500-AUDIT-THE-CHANGE
           IF WS-RC < 4
               MOVE SPACES TO WS-MSG
               STRING "Quote " FUNCTION TRIM(SQ-QUOTE-NUMBER)
                      " converted to order "
                      FUNCTION TRIM(SQ-ORDER-NUMBER)
                      " with the accepted terms retained"
                   DELIMITED BY SIZE INTO WS-MSG
           END-IF.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   TEMPLATE - SO-024.  Commercial content only.  Cancelled,
      *>   despatched, billed and returned quantities start at zero,
      *>   the order is DRAFT and no deposit is treated as received,
      *>   so a copy can never look part shipped or part paid.
       3000-TEMPLATE-COPY SECTION.
       3000-START.
           IF SQ-SOURCE-TYPE = "QUOTE"
               MOVE SQ-SOURCE-REF TO SQ-QUOTE-NUMBER
               MOVE 0 TO SQ-VERSION
               PERFORM 0300-RESOLVE-VERSION
               IF WS-RC > 4
                   GO TO 3000-EXIT
               END-IF
               PERFORM 0310-READ-QUOTE
               IF WS-RC > 4
                   GO TO 3000-EXIT
               END-IF
               PERFORM 0400-READ-QUOTE-LINES
               PERFORM 6600-ORDER-FROM-QUOTE
               MOVE SPACES TO WS-O-QUOTE-REF
           ELSE
               PERFORM 3100-READ-SOURCE-ORDER
               IF WS-RC > 4
                   GO TO 3000-EXIT
               END-IF
               PERFORM 3200-READ-SOURCE-LINES
               IF WS-RC > 4
                   GO TO 3000-EXIT
               END-IF
           END-IF
           IF SQ-LINE-CNT = 0
               MOVE "SOQ0013" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "The source document has no lines to copy"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 3000-EXIT
           END-IF
           MOVE "STANDARD" TO WS-O-TYPE
           MOVE SPACES TO WS-O-BLANKET
           MOVE SPACES TO WS-O-QUOTE-REF
           IF SQ-CUSTOMER NOT = SPACES
               MOVE SQ-CUSTOMER TO WS-O-CUSTOMER
           END-IF
           PERFORM 6400-TOTAL-THE-LINES
           PERFORM 0500-DUPLICATE-REF-CHECK
           MOVE "SORD"    TO WS-SERIES
           PERFORM 6000-NEXT-NUMBER
           IF WS-RC > 4
               GO TO 3000-EXIT
           END-IF
           MOVE WS-NEWNUM TO SQ-ORDER-NUMBER
           PERFORM 8000-BEGIN-TRAN
           PERFORM 7000-INSERT-ORDER-HDR
           IF WS-RC > 4
               GO TO 3000-EXIT
           END-IF
           PERFORM 7100-INSERT-ORDER-LINES
           IF WS-RC > 4
               GO TO 3000-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           PERFORM 7500-AUDIT-THE-CHANGE
           IF WS-RC < 4
               MOVE SPACES TO WS-MSG
               STRING "Order " FUNCTION TRIM(SQ-ORDER-NUMBER)
                      " created as a new draft from "
                      FUNCTION TRIM(SQ-SOURCE-REF)
                      "; no fulfilment or payment state was copied"
                   DELIMITED BY SIZE INTO WS-MSG
           END-IF.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       3100-READ-SOURCE-ORDER SECTION.
       3100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT 'STANDARD', ISNULL(o.QuoteNumber,''), '', "
                  "o.CustomerCode, ISNULL(o.InvoiceCustomerCode,''), "
                  "o.CurrencyCode, CONVERT(varchar(24), o.FxRate), "
                  "ISNULL(o.TermsCode,''), ISNULL(o.TermsSnapshot,''),"
                  " ISNULL(o.SalespersonCode,''), "
                  "ISNULL(o.TermsAndConditionsVersion,''), "
                  "ISNULL(o.IncotermsRule,''), "
                  "ISNULL(o.IncotermsNamedPlace,''), "
                  "ISNULL(o.DeliveryAddressRef,''), "
                  "ISNULL(o.DeliveryAddressSnapshot,''), "
                  "ISNULL(o.BillToAddressSnapshot,''), "
                  "ISNULL(o.VatTreatmentBasis,''), "
                  "ISNULL(o.CustomerVatRegNo,''), "
                  "ISNULL(o.VatLocation,''), o.SiteCode, "
                  "CONVERT(varchar(1), o.RetentionOfTitle) FROM "
                  "so.SalesOrder o WHERE o.CompanyCode = ? AND "
                  "o.OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SQ-SOURCE-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 21 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 30 TO SQLIO-COL-WIDTH(3)
           MOVE 20 TO SQLIO-COL-WIDTH(4)
           MOVE 20 TO SQLIO-COL-WIDTH(5)
           MOVE 3 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 12 TO SQLIO-COL-WIDTH(8)
           MOVE 100 TO SQLIO-COL-WIDTH(9)
           MOVE 20 TO SQLIO-COL-WIDTH(10)
           MOVE 20 TO SQLIO-COL-WIDTH(11)
           MOVE 3 TO SQLIO-COL-WIDTH(12)
           MOVE 60 TO SQLIO-COL-WIDTH(13)
           MOVE 10 TO SQLIO-COL-WIDTH(14)
           MOVE 300 TO SQLIO-COL-WIDTH(15)
           MOVE 300 TO SQLIO-COL-WIDTH(16)
           MOVE 20 TO SQLIO-COL-WIDTH(17)
           MOVE 15 TO SQLIO-COL-WIDTH(18)
           MOVE 3 TO SQLIO-COL-WIDTH(19)
           MOVE 10 TO SQLIO-COL-WIDTH(20)
           MOVE 1 TO SQLIO-COL-WIDTH(21)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ORD
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 3100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO WS-RC
               MOVE "SOQ0002" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Source order " FUNCTION TRIM(SQ-SOURCE-REF)
                      " is not held" DELIMITED BY SIZE INTO WS-MSG
               ADD 1 TO WS-MSGCNT
               GO TO 3100-EXIT
           END-IF
           MOVE WS-O-CUSTOMER TO SQ-CUSTOMER
           MOVE WS-O-CURRENCY TO SQ-CURRENCY
           COMPUTE SQ-FX-RATE = FUNCTION NUMVAL(WS-O-FX)
           MOVE WS-O-TERMS TO SQ-TERMS-CODE
           MOVE WS-O-SALESPERSON TO SQ-SALESPERSON
           IF SQ-SITE-CODE = SPACES
               MOVE WS-O-SITE TO SQ-SITE-CODE
           END-IF.
       3100-EXIT.
           EXIT.

      *>   so.SalesOrderLine mapped into the same line table.  Note
      *>   what is NOT selected: cancelled, despatched, billed and
      *>   returned quantities never travel with a template.
       3200-READ-SOURCE-LINES SECTION.
       3200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(10), l.LineNum), "
                  "ISNULL(l.ItemCode,''), ISNULL(l.Revision,''), "
                  "l.Description, CONVERT(varchar(24), l.AcceptedQty "
                  "- l.CancelledQty), ISNULL(l.UomCode,''), "
                  "CONVERT(varchar(24), l.UnitPrice), "
                  "CONVERT(varchar(24), l.PricePerQty), "
                  "CONVERT(varchar(12), l.DiscountPct1), "
                  "CONVERT(varchar(12), l.DiscountPct2), "
                  "CONVERT(varchar(24), l.NetAmount), "
                  "ISNULL(l.VatCode,''), CONVERT(varchar(24), "
                  "l.VatAmount), CONVERT(varchar(24), "
                  "ISNULL(l.CostSnapshotGBP,0)), "
                  "ISNULL(l.CostBasis,''), ISNULL(l.PriceSource,'') "
                  "FROM so.SalesOrderLine l WHERE l.CompanyCode = ? "
                  "AND l.OrderNumber = ? AND l.Status <> 'CANCELLED' "
                  "ORDER BY l.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SQ-SOURCE-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           PERFORM 0450-SET-LINE-COLS
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-QLIN
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 3200-EXIT
           END-IF
           PERFORM 0460-FETCH-LINE-LOOP
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-QLIN.
       3200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   CALLOFF - SO-024.  A call-off may not silently exceed the
      *>   blanket's remaining approved quantity or value.  Without an
      *>   approved blanket change the answer is 06 and no demand.
       4000-BLANKET-CALLOFF SECTION.
       4000-START.
           PERFORM 4100-READ-BLANKET
           IF WS-RC > 4
               GO TO 4000-EXIT
           END-IF
           PERFORM 4200-BLANKET-CONSUMED
           IF WS-RC > 4
               GO TO 4000-EXIT
           END-IF
           COMPUTE SQ-BLANKET-FREE-QTY =
                   SQ-BLANKET-QTY - SQ-BLANKET-USED-QTY
           COMPUTE SQ-BLANKET-FREE-VAL =
                   SQ-BLANKET-VALUE - SQ-BLANKET-USED-VAL
           IF SQ-CALLOFF-QTY = 0 AND SQ-LINE-CNT > 0
               PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > SQ-LINE-CNT
                   ADD SQ-L-QTY(WS-I) TO SQ-CALLOFF-QTY
               END-PERFORM
           END-IF
           PERFORM 6400-TOTAL-THE-LINES
           IF SQ-CALLOFF-VALUE = 0
               MOVE SQ-NET-AMOUNT TO SQ-CALLOFF-VALUE
           END-IF
           IF SQ-CALLOFF-QTY > SQ-BLANKET-FREE-QTY OR
              SQ-CALLOFF-VALUE > SQ-BLANKET-FREE-VAL
               PERFORM 4300-CALLOFF-EXCEEDED
               IF WS-RC > 4
                   GO TO 4000-EXIT
               END-IF
           END-IF
           IF SQ-LINE-CNT = 0
               MOVE "SOQ0013" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A call-off needs at least one line"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 4000-EXIT
           END-IF
           PERFORM 0500-DUPLICATE-REF-CHECK
           PERFORM 6600-ORDER-FROM-QUOTE
           MOVE SPACES TO WS-O-QUOTE-REF
           MOVE "CALLOFF" TO WS-O-TYPE
           MOVE SQ-BLANKET-NUMBER TO WS-O-BLANKET
           MOVE "SORD"    TO WS-SERIES
           PERFORM 6000-NEXT-NUMBER
           IF WS-RC > 4
               GO TO 4000-EXIT
           END-IF
           MOVE WS-NEWNUM TO SQ-ORDER-NUMBER
           PERFORM 8000-BEGIN-TRAN
           PERFORM 7000-INSERT-ORDER-HDR
           IF WS-RC > 4
               GO TO 4000-EXIT
           END-IF
           PERFORM 7100-INSERT-ORDER-LINES
           IF WS-RC > 4
               GO TO 4000-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           COMPUTE SQ-BLANKET-FREE-QTY =
                   SQ-BLANKET-FREE-QTY - SQ-CALLOFF-QTY
           COMPUTE SQ-BLANKET-FREE-VAL =
                   SQ-BLANKET-FREE-VAL - SQ-CALLOFF-VALUE
           PERFORM 7500-AUDIT-THE-CHANGE
           IF WS-RC < 4
               MOVE SQ-BLANKET-FREE-QTY TO WS-UK-QTY
               MOVE SPACES TO WS-MSG
               STRING "Call-off " FUNCTION TRIM(SQ-ORDER-NUMBER)
                      " raised against blanket "
                      FUNCTION TRIM(SQ-BLANKET-NUMBER)
                      "; remaining approved quantity "
                      FUNCTION TRIM(WS-UK-QTY)
                   DELIMITED BY SIZE INTO WS-MSG
           END-IF.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       4100-READ-BLANKET SECTION.
       4100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT o.CommercialStatus, o.CustomerCode, "
                  "o.CurrencyCode, CONVERT(varchar(24), ISNULL("
                  "(SELECT SUM(b.AcceptedQty - b.CancelledQty) FROM "
                  "so.SalesOrderLine b WHERE b.SalesOrderId = "
                  "o.SalesOrderId AND b.Status <> 'CANCELLED'),0)), "
                  "CONVERT(varchar(24), o.NetAmount) FROM "
                  "so.SalesOrder o WHERE o.CompanyCode = ? AND "
                  "o.OrderNumber = ? AND o.OrderType = 'BLANKET'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SQ-BLANKET-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 3 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 24 TO SQLIO-COL-WIDTH(5)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BLK
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 4100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO WS-RC
               MOVE "SOQ0008" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Blanket order "
                      FUNCTION TRIM(SQ-BLANKET-NUMBER)
                      " is not held for this company"
                   DELIMITED BY SIZE INTO WS-MSG
               ADD 1 TO WS-MSGCNT
               GO TO 4100-EXIT
           END-IF
           MOVE WS-B-STATUS TO SQ-BLANKET-STATUS
           IF WS-B-STATUS NOT = "ACCEPTED" AND
              WS-B-STATUS NOT = "AMENDED"
               MOVE "SOQ0008" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Blanket order "
                      FUNCTION TRIM(SQ-BLANKET-NUMBER)
                      " is " FUNCTION TRIM(WS-B-STATUS)
                      " - it must be accepted before a call-off"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 4100-EXIT
           END-IF
           IF SQ-CUSTOMER = SPACES
               MOVE WS-B-CUSTOMER TO SQ-CUSTOMER
           END-IF
           IF SQ-CURRENCY = SPACES
               MOVE WS-B-CURRENCY TO SQ-CURRENCY
           END-IF
           COMPUTE SQ-BLANKET-QTY = FUNCTION NUMVAL(WS-B-QTY)
           COMPUTE SQ-BLANKET-VALUE = FUNCTION NUMVAL(WS-B-VALUE).
       4100-EXIT.
           EXIT.

      *>   everything already called off, cancelled call-offs excluded
       4200-BLANKET-CONSUMED SECTION.
       4200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), ISNULL(SUM("
                  "l.AcceptedQty - l.CancelledQty),0)), "
                  "CONVERT(varchar(24), ISNULL(SUM(l.NetAmount),0)) "
                  "FROM so.SalesOrderLine l JOIN so.SalesOrder o ON "
                  "o.SalesOrderId = l.SalesOrderId WHERE "
                  "o.CompanyCode = ? AND o.BlanketOrderNumber = ? "
                  "AND o.OrderType = 'CALLOFF' AND "
                  "o.CommercialStatus <> 'CANCELLED' AND l.Status <> "
                  "'CANCELLED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SQ-BLANKET-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-FOUR
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 4200-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 0 TO SQ-BLANKET-USED-QTY SQ-BLANKET-USED-VAL
           ELSE
               COMPUTE SQ-BLANKET-USED-QTY = FUNCTION NUMVAL(WS-F1)
               COMPUTE SQ-BLANKET-USED-VAL = FUNCTION NUMVAL(WS-F2)
           END-IF.
       4200-EXIT.
           EXIT.

      *>   SOQ0009 - blocked pending an approved blanket change.  An
      *>   approval reference turns the block into a logged warning.
       4300-CALLOFF-EXCEEDED SECTION.
       4300-START.
           MOVE "SOQ0009" TO WS-REASON
           MOVE SQ-BLANKET-FREE-QTY TO WS-UK-QTY
           MOVE SQ-BLANKET-FREE-VAL TO WS-UK-MONEY
           MOVE SPACES TO WS-MSG
           STRING "Call-off exceeds the blanket's remaining approved "
                  "quantity " FUNCTION TRIM(WS-UK-QTY) " or value "
                  FUNCTION TRIM(WS-UK-MONEY)
               DELIMITED BY SIZE INTO WS-MSG
           ADD 1 TO WS-MSGCNT
           IF SQ-APPROVAL-ID = 0
               MOVE 6 TO WS-RC
               STRING FUNCTION TRIM(WS-MSG)
                      " - blocked pending an approved blanket change"
                   DELIMITED BY SIZE INTO WS-MSG
           ELSE
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
               MOVE SQ-APPROVAL-ID TO WS-INT-ED
               STRING FUNCTION TRIM(WS-MSG)
                      " - allowed under blanket change approval "
                      FUNCTION TRIM(WS-INT-ED)
                   DELIMITED BY SIZE INTO WS-MSG
           END-IF.
       4300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   SHARED SERVICE AND POSTING ROUTINES
      *>----------------------------------------------------------------
       6000-NEXT-NUMBER SECTION.
       6000-START.
           INITIALIZE WS-NUM-CA
           MOVE "NEXT" TO CA-FUNCTION OF WS-NUM-CA
           MOVE WS-CO TO CA-COMPANY OF WS-NUM-CA
           MOVE WS-SITE TO CA-SITE OF WS-NUM-CA
           MOVE WS-USER TO CA-USER OF WS-NUM-CA
           MOVE WS-BUSDATE TO CA-BUSINESS-DATE OF WS-NUM-CA
           MOVE WS-CORREL TO CA-CORRELATION-ID OF WS-NUM-CA
           MOVE WS-SERIES TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUM-CA
           IF CA-RETURN-CODE OF WS-NUM-CA > 4
               MOVE 12 TO WS-RC
               MOVE "SOQ0010" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Number series " FUNCTION TRIM(WS-SERIES)
                      " refused a number - "
                      FUNCTION TRIM(CA-MESSAGE OF WS-NUM-CA)
                   DELIMITED BY SIZE INTO WS-MSG
               ADD 1 TO WS-MSGCNT
               GO TO 6000-EXIT
           END-IF
           MOVE NG-DOCUMENT-NUMBER TO WS-NEWNUM.
       6000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       6100-INSERT-QUOTE-HDR SECTION.
       6100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO so.Quote (CompanyCode, QuoteNumber, "
                  "Version, CustomerCode, ProspectName, QuoteDate, "
                  "ValidUntil, CurrencyCode, FxRate, TermsCode, "
                  "TermsVersion, DeliveryEstimate, Assumptions, "
                  "SalespersonCode, NetAmount, VatAmount, "
                  "GrossAmount, CostBasisGBP, MarginPct, Status, "
                  "CorrelationId, CreatedBy) VALUES (?, ?, "
                  "CONVERT(smallint, ?), NULLIF(?,''), NULLIF(?,''), "
                  "CONVERT(date, ?, 112), CONVERT(date, ?, 112), ?, "
                  "CONVERT(decimal(18,8), ?), NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(?,''), NULLIF(?,''), "
                  "NULLIF(?,''), CONVERT(decimal(18,2), ?), "
                  "CONVERT(decimal(18,2), ?), "
                  "CONVERT(decimal(18,2), ?), "
                  "CONVERT(decimal(18,2), ?), "
                  "CONVERT(decimal(9,4), ?), ?, NULLIF(?,''), ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 22 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SQ-QUOTE-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE SQ-VERSION TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE SQ-CUSTOMER TO SQLIO-PARM-VAL(4)
           MOVE SQ-PROSPECT-NAME TO SQLIO-PARM-VAL(5)
           MOVE SQ-QUOTE-DATE TO SQLIO-PARM-VAL(6)
           MOVE SQ-VALID-UNTIL TO SQLIO-PARM-VAL(7)
           MOVE SQ-CURRENCY TO SQLIO-PARM-VAL(8)
           MOVE SQ-FX-RATE TO WS-RATE-ED
           MOVE FUNCTION TRIM(WS-RATE-ED) TO SQLIO-PARM-VAL(9)
           MOVE SQ-TERMS-CODE TO SQLIO-PARM-VAL(10)
           MOVE SQ-TERMS-VERSION TO SQLIO-PARM-VAL(11)
           MOVE SQ-DELIVERY-EST TO SQLIO-PARM-VAL(12)
           MOVE SQ-ASSUMPTIONS TO SQLIO-PARM-VAL(13)
           MOVE SQ-SALESPERSON TO SQLIO-PARM-VAL(14)
           MOVE SQ-NET-AMOUNT TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(15)
           MOVE SQ-VAT-AMOUNT TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(16)
           MOVE SQ-GROSS-AMOUNT TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(17)
           MOVE SQ-COST-BASIS-GBP TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(18)
           MOVE SQ-MARGIN-PCT TO WS-PCT-ED
           MOVE FUNCTION TRIM(WS-PCT-ED) TO SQLIO-PARM-VAL(19)
           MOVE SQ-STATUS TO SQLIO-PARM-VAL(20)
           MOVE WS-CORREL TO SQLIO-PARM-VAL(21)
           MOVE WS-USER TO SQLIO-PARM-VAL(22)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 22
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS.
       6100-EXIT.
           EXIT.

       6150-READ-QUOTE-ID SECTION.
       6150-START.
           IF WS-RC > 4
               GO TO 6150-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), QuoteId), '' FROM "
                  "so.Quote WHERE CompanyCode = ? AND QuoteNumber = ?"
                  " AND Version = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SQ-QUOTE-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE SQ-VERSION TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 6150-EXIT
           END-IF
           COMPUTE SQ-QUOTE-ID = FUNCTION NUMVAL(WS-T1).
       6150-EXIT.
           EXIT.

       6200-INSERT-QUOTE-LINES SECTION.
       6200-START.
           IF WS-RC > 4
               GO TO 6200-EXIT
           END-IF
           MOVE 0 TO SQ-LINES-CREATED
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > SQ-LINE-CNT OR WS-RC > 4
               PERFORM 6250-INSERT-ONE-QLINE
           END-PERFORM.
       6200-EXIT.
           EXIT.

       6250-INSERT-ONE-QLINE SECTION.
       6250-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO so.QuoteLine (QuoteId, LineNum, "
                  "ItemCode, Revision, Description, Quantity, "
                  "UomCode, UnitPrice, PricePerQty, DiscountPct1, "
                  "DiscountPct2, NetAmount, VatCode, VatAmount, "
                  "EstimatedCostGBP, CostBasis, MarginPct, "
                  "LeadTimeDays) VALUES (?, CONVERT(int, ?), "
                  "NULLIF(?,''), NULLIF(?,''), ?, "
                  "CONVERT(decimal(19,6), ?), NULLIF(?,''), "
                  "CONVERT(decimal(19,6), ?), "
                  "CONVERT(decimal(19,6), ?), "
                  "CONVERT(decimal(9,4), ?), CONVERT(decimal(9,4), ?)"
                  ", CONVERT(decimal(18,2), ?), NULLIF(?,''), "
                  "CONVERT(decimal(18,2), ?), "
                  "CONVERT(decimal(18,2), ?), NULLIF(?,''), "
                  "CONVERT(decimal(9,4), ?), CONVERT(smallint, ?))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 18 TO SQLIO-PARM-CNT
           MOVE SQ-QUOTE-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE SQ-L-LINE-NUM(WS-I) TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(2)
           MOVE SQ-L-ITEM(WS-I) TO SQLIO-PARM-VAL(3)
           MOVE SQ-L-REVISION(WS-I) TO SQLIO-PARM-VAL(4)
           MOVE SQ-L-DESC(WS-I) TO SQLIO-PARM-VAL(5)
           MOVE SQ-L-QTY(WS-I) TO WS-QTY-ED
           MOVE FUNCTION TRIM(WS-QTY-ED) TO SQLIO-PARM-VAL(6)
           MOVE SQ-L-UOM(WS-I) TO SQLIO-PARM-VAL(7)
           MOVE SQ-L-UNIT-PRICE(WS-I) TO WS-PRICE-ED
           MOVE FUNCTION TRIM(WS-PRICE-ED) TO SQLIO-PARM-VAL(8)
           MOVE SQ-L-PRICE-PER(WS-I) TO WS-PRICE-ED
           MOVE FUNCTION TRIM(WS-PRICE-ED) TO SQLIO-PARM-VAL(9)
           MOVE SQ-L-DISC1(WS-I) TO WS-PCT-ED
           MOVE FUNCTION TRIM(WS-PCT-ED) TO SQLIO-PARM-VAL(10)
           MOVE SQ-L-DISC2(WS-I) TO WS-PCT-ED
           MOVE FUNCTION TRIM(WS-PCT-ED) TO SQLIO-PARM-VAL(11)
           MOVE SQ-L-NET(WS-I) TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(12)
           MOVE SQ-L-VAT-CODE(WS-I) TO SQLIO-PARM-VAL(13)
           MOVE SQ-L-VAT-AMT(WS-I) TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(14)
           MOVE SQ-L-COST-GBP(WS-I) TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(15)
           MOVE SQ-L-COST-BASIS(WS-I) TO SQLIO-PARM-VAL(16)
           MOVE SQ-L-MARGIN-PCT(WS-I) TO WS-PCT-ED
           MOVE FUNCTION TRIM(WS-PCT-ED) TO SQLIO-PARM-VAL(17)
           MOVE SQ-L-LEAD-DAYS(WS-I) TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(18)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 18
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS
           IF WS-RC < 8
               ADD 1 TO SQ-LINES-CREATED
           END-IF.
       6250-EXIT.
           EXIT.

      *>   one status move on one version - used by ISSUE, DECLINE,
      *>   EXPIRE and by REVISE to retire the version it replaces
       6300-SET-VERSION-STATUS SECTION.
       6300-START.
           IF WS-RC > 4
               GO TO 6300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.Quote SET Status = ?, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE CompanyCode = ? "
                  "AND QuoteNumber = ? AND Version = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE WS-NEW-STATUS TO SQLIO-PARM-VAL(1)
           MOVE WS-USER TO SQLIO-PARM-VAL(2)
           MOVE WS-CO TO SQLIO-PARM-VAL(3)
           MOVE SQ-QUOTE-NUMBER TO SQLIO-PARM-VAL(4)
           MOVE WS-J TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS.
       6300-EXIT.
           EXIT.

      *>   line extension and document totals.  The sequence is the
      *>   one in specification 05 section 5: quantity to the price
      *>   unit, extend by price-per, then the percentage discounts in
      *>   order.  VAT amounts are supplied by the pricing and VAT
      *>   services and are only summed here.
       6400-TOTAL-THE-LINES SECTION.
       6400-START.
           MOVE 0 TO SQ-NET-AMOUNT SQ-VAT-AMOUNT SQ-GROSS-AMOUNT
           MOVE 0 TO WS-COST-TOT
           MOVE 0 TO SQ-COST-BASIS-GBP SQ-MARGIN-PCT
           MOVE "Y" TO SQ-MARGIN-DEFINED
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > SQ-LINE-CNT
               IF SQ-L-LINE-NUM(WS-I) = 0
                   COMPUTE SQ-L-LINE-NUM(WS-I) = WS-I * 10
               END-IF
               IF SQ-L-PRICE-PER(WS-I) = 0
                   MOVE 1 TO SQ-L-PRICE-PER(WS-I)
               END-IF
               IF SQ-L-NET(WS-I) = 0 AND SQ-L-UNIT-PRICE(WS-I) NOT = 0
                   MOVE SQ-L-PRICE-PER(WS-I) TO WS-PPQ
                   COMPUTE WS-AMT1 ROUNDED = SQ-L-QTY(WS-I) / WS-PPQ
                           * SQ-L-UNIT-PRICE(WS-I)
                   COMPUTE WS-AMT2 ROUNDED = WS-AMT1 *
                           (1 - SQ-L-DISC1(WS-I) / 100)
                   COMPUTE WS-AMT3 ROUNDED = WS-AMT2 *
                           (1 - SQ-L-DISC2(WS-I) / 100)
                   MOVE WS-AMT3 TO SQ-L-NET(WS-I)
               END-IF
               ADD SQ-L-NET(WS-I) TO SQ-NET-AMOUNT
               ADD SQ-L-VAT-AMT(WS-I) TO SQ-VAT-AMOUNT
               ADD SQ-L-COST-GBP(WS-I) TO WS-COST-TOT
               IF SQ-L-NET(WS-I) NOT = 0 AND
                  SQ-L-COST-GBP(WS-I) NOT = 0
                   COMPUTE SQ-L-MARGIN-PCT(WS-I) ROUNDED =
                       (SQ-L-NET(WS-I) - SQ-L-COST-GBP(WS-I))
                       / SQ-L-NET(WS-I) * 100
               END-IF
           END-PERFORM
           COMPUTE SQ-GROSS-AMOUNT = SQ-NET-AMOUNT + SQ-VAT-AMOUNT
           MOVE WS-COST-TOT TO SQ-COST-BASIS-GBP
      *>       a zero denominator or an absent cost is "not defined"
      *>       and goes to review - it is never reported as zero
           IF SQ-NET-AMOUNT = 0 OR WS-COST-TOT = 0
               MOVE "N" TO SQ-MARGIN-DEFINED
               MOVE 0 TO SQ-MARGIN-PCT
           ELSE
               COMPUTE SQ-MARGIN-PCT ROUNDED =
                   (SQ-NET-AMOUNT - WS-COST-TOT) / SQ-NET-AMOUNT * 100
           END-IF.
       6400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   COM-015 - the correlation id owns the instruction.  A
      *>   second attempt is told the outcome, never allowed to make
      *>   a second order.
       6500-CLAIM-IDEMPOTENCY SECTION.
       6500-START.
           IF WS-CORREL = SPACES
               GO TO 6500-EXIT
           END-IF
           MOVE SPACES TO WS-OPID
           MOVE SQ-VERSION TO WS-VER-ED
           STRING "SOQUOTE CONVERT " FUNCTION TRIM(SQ-QUOTE-NUMBER)
                  " V" FUNCTION TRIM(WS-VER-ED)
               DELIMITED BY SIZE INTO WS-OPID
           INITIALIZE WS-IDEM-CA
           MOVE "BEGIN" TO CA-FUNCTION OF WS-IDEM-CA
           MOVE WS-CO TO CA-COMPANY OF WS-IDEM-CA
           MOVE WS-USER TO CA-USER OF WS-IDEM-CA
           MOVE WS-BUSDATE TO CA-BUSINESS-DATE OF WS-IDEM-CA
           MOVE WS-CORREL TO CA-CORRELATION-ID OF WS-IDEM-CA
           MOVE WS-OPID TO ID-OPERATION
           CALL "IDEMPOT" USING WS-IDEM-CA
           EVALUATE CA-RETURN-CODE OF WS-IDEM-CA
               WHEN 0
                   MOVE "Y" TO WS-IDEM-OPEN
               WHEN 4
                   MOVE ID-RESULT-REF TO SQ-ORDER-NUMBER
                   PERFORM 9400-ALREADY-CONVERTED
               WHEN 6
                   MOVE 6 TO WS-RC
                   MOVE "SOQ0012" TO WS-REASON
                   MOVE SPACES TO WS-MSG
                   STRING "Another attempt at this conversion is "
                          "still running - the outcome is not yet "
                          "known" DELIMITED BY SIZE INTO WS-MSG
                   ADD 1 TO WS-MSGCNT
               WHEN OTHER
                   MOVE 8 TO WS-RC
                   MOVE "SOQ0012" TO WS-REASON
                   MOVE CA-MESSAGE OF WS-IDEM-CA TO WS-MSG
                   ADD 1 TO WS-MSGCNT
           END-EVALUATE.
       6500-EXIT.
           EXIT.

       6550-END-IDEMPOTENCY SECTION.
       6550-START.
           IF WS-IDEM-OPEN NOT = "Y"
               GO TO 6550-EXIT
           END-IF
           MOVE "END" TO CA-FUNCTION OF WS-IDEM-CA
           MOVE "COMPLETED" TO ID-STATUS
           MOVE SQ-ORDER-NUMBER TO ID-RESULT-REF
           MOVE SPACES TO ID-RESULT-MESSAGE
           STRING "Order " FUNCTION TRIM(SQ-ORDER-NUMBER)
                  " created from quote "
                  FUNCTION TRIM(SQ-QUOTE-NUMBER)
               DELIMITED BY SIZE INTO ID-RESULT-MESSAGE
           CALL "IDEMPOT" USING WS-IDEM-CA
           MOVE "N" TO WS-IDEM-OPEN.
       6550-EXIT.
           EXIT.

      *>   the accepted commercial terms carried onto the order
       6600-ORDER-FROM-QUOTE SECTION.
       6600-START.
           MOVE "STANDARD" TO WS-O-TYPE
           MOVE SQ-QUOTE-NUMBER TO WS-O-QUOTE-REF
           MOVE SPACES TO WS-O-BLANKET
           MOVE SQ-CUSTOMER TO WS-O-CUSTOMER
           MOVE SPACES TO WS-O-INV-CUST
           MOVE SQ-CURRENCY TO WS-O-CURRENCY
           MOVE SQ-FX-RATE TO WS-RATE-ED
           MOVE FUNCTION TRIM(WS-RATE-ED) TO WS-O-FX
           MOVE SQ-TERMS-CODE TO WS-O-TERMS
           MOVE SQ-DELIVERY-EST TO WS-O-TERMS-SNAP
           MOVE SQ-SALESPERSON TO WS-O-SALESPERSON
           MOVE SQ-TERMS-VERSION TO WS-O-TC-VERSION
           MOVE SQ-SITE-CODE TO WS-O-SITE
           MOVE "1" TO WS-O-ROT
           PERFORM 6700-DEFAULT-ORDER-HEADER.
       6600-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   CR2601.  A quotation prices goods; it does not re-state the
      *>   account.  So when the quote becomes an order the invoiced
      *>   party, the delivery and billing addresses, the payment
      *>   terms, the trading terms in issue and the basis of the VAT
      *>   treatment are taken from the sales ledger, exactly as the
      *>   order desk would take them off the account card.  A value
      *>   the caller has already stated is never overwritten, and
      *>   what was used is given back in SQ-ORDER-DEFAULTS so the
      *>   acknowledgement can show it.
       6700-DEFAULT-ORDER-HEADER SECTION.
       6700-START.
           PERFORM 6710-CALLER-VALUES-FIRST
           IF WS-O-CUSTOMER = SPACES
      *>       a prospect quotation has no account to read
               GO TO 6700-EXIT
           END-IF
           PERFORM 6720-READ-CUSTOMER
           IF WS-RC > 4
               GO TO 6700-EXIT
           END-IF
           IF WS-CUST-FOUND = "Y"
               PERFORM 6730-ACCOUNT-DEFAULTS
               PERFORM 6740-ADDRESS-DEFAULTS
               PERFORM 6760-TERMS-AND-VAT
           END-IF
           PERFORM 6780-PUBLISH-DEFAULTS.
       6700-EXIT.
           EXIT.

       6710-CALLER-VALUES-FIRST SECTION.
       6710-START.
           MOVE "M" TO SQ-DEFAULTS-SRC
           IF SQ-INVOICE-CUST NOT = SPACES
               MOVE SQ-INVOICE-CUST TO WS-O-INV-CUST
               MOVE "C" TO SQ-DEFAULTS-SRC
           END-IF
           IF SQ-DELIV-REF NOT = SPACES
               MOVE SQ-DELIV-REF TO WS-O-ADDR-REF
               MOVE "C" TO SQ-DEFAULTS-SRC
           END-IF
           IF SQ-DELIV-SNAP NOT = SPACES
               MOVE SQ-DELIV-SNAP TO WS-O-ADDR-SNAP
               MOVE "C" TO SQ-DEFAULTS-SRC
           END-IF
           IF SQ-BILL-SNAP NOT = SPACES
               MOVE SQ-BILL-SNAP TO WS-O-BILL-SNAP
           END-IF
           IF SQ-VAT-BASIS NOT = SPACES
               MOVE SQ-VAT-BASIS TO WS-O-VAT-BASIS
               MOVE "C" TO SQ-DEFAULTS-SRC
           END-IF
           IF SQ-VAT-LOCATION NOT = SPACES
               MOVE SQ-VAT-LOCATION TO WS-O-VAT-LOC
           END-IF
           IF SQ-VAT-REG-NO NOT = SPACES
               MOVE SQ-VAT-REG-NO TO WS-O-VAT-REG
           END-IF
           IF SQ-INCOTERMS NOT = SPACES
               MOVE SQ-INCOTERMS TO WS-O-INCO
           END-IF
           IF SQ-INCO-PLACE NOT = SPACES
               MOVE SQ-INCO-PLACE TO WS-O-INCO-PLACE
           END-IF.
       6710-EXIT.
           EXIT.

       6720-READ-CUSTOMER SECTION.
       6720-START.
           MOVE "N" TO WS-CUST-FOUND
           INITIALIZE WS-CUST
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(NULLIF(c.InvoiceCustomerCode,''), "
                  "c.CustomerCode), ISNULL(c.TermsCode,''), "
                  "ISNULL(c.VatLocation,'GB'), ISNULL(c.VatRegNo,"
                  "''), ISNULL(c.SalespersonCode,''), "
                  "ISNULL(c.DeliveryTermsCode,'') FROM ar.Customer c"
                  " WHERE c.CompanyCode = ? AND c.CustomerCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-O-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 3 TO SQLIO-COL-WIDTH(3)
           MOVE 15 TO SQLIO-COL-WIDTH(4)
           MOVE 20 TO SQLIO-COL-WIDTH(5)
           MOVE 10 TO SQLIO-COL-WIDTH(6)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CUST
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 6720-EXIT
           END-IF
           IF WS-CU-INV-CUST NOT = SPACES
               MOVE "Y" TO WS-CUST-FOUND
           END-IF.
       6720-EXIT.
           EXIT.

       6730-ACCOUNT-DEFAULTS SECTION.
       6730-START.
           IF WS-O-INV-CUST = SPACES
               MOVE WS-CU-INV-CUST TO WS-O-INV-CUST
           END-IF
           IF WS-O-SALESPERSON = SPACES
               MOVE WS-CU-SALESPERSON TO WS-O-SALESPERSON
           END-IF
           IF WS-O-VAT-LOC = SPACES
               MOVE WS-CU-VAT-LOC TO WS-O-VAT-LOC
           END-IF
           IF WS-O-VAT-REG = SPACES
               MOVE WS-CU-VAT-REG TO WS-O-VAT-REG
           END-IF.
       6730-EXIT.
           EXIT.

      *>   the address the goods go to, and the one the invoice goes
      *>   to - snapshotted, because the account card may be changed
      *>   after the order is taken (SO-002)
       6740-ADDRESS-DEFAULTS SECTION.
       6740-START.
           IF WS-O-ADDR-SNAP = SPACES
               MOVE "DELIVERY" TO WS-ADDR-ROLE
               PERFORM 6750-READ-ADDRESS
               IF WS-AD-SNAP NOT = SPACES
                   MOVE WS-AD-SNAP TO WS-O-ADDR-SNAP
                   IF WS-O-ADDR-REF = SPACES
                       MOVE WS-AD-REF TO WS-O-ADDR-REF
                   END-IF
               END-IF
           END-IF
           IF WS-O-BILL-SNAP = SPACES
               MOVE "BILLING" TO WS-ADDR-ROLE
               PERFORM 6750-READ-ADDRESS
               IF WS-AD-SNAP = SPACES
                   MOVE "REGISTERED" TO WS-ADDR-ROLE
                   PERFORM 6750-READ-ADDRESS
               END-IF
               IF WS-AD-SNAP NOT = SPACES
                   MOVE WS-AD-SNAP TO WS-O-BILL-SNAP
               ELSE
                   MOVE WS-O-ADDR-SNAP TO WS-O-BILL-SNAP
               END-IF
           END-IF
           IF WS-O-INCO-PLACE = SPACES AND WS-O-ADDR-SNAP NOT = SPACES
               MOVE WS-O-ADDR-SNAP(1:60) TO WS-O-INCO-PLACE
           END-IF.
       6740-EXIT.
           EXIT.

       6750-READ-ADDRESS SECTION.
       6750-START.
           INITIALIZE WS-ADR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) a.AddressRef, LTRIM(RTRIM("
                  "CONCAT(ISNULL(NULLIF(a.Name,'') + ', ',''), "
                  "a.Line1, ISNULL(', ' + NULLIF(a.Line2,''),''), "
                  "ISNULL(', ' + NULLIF(a.Town,''),''), "
                  "ISNULL(', ' + NULLIF(a.Postcode,''),'')))) FROM "
                  "ar.CustomerAddress a WHERE a.CompanyCode = ? AND "
                  "a.CustomerCode = ? AND a.AddressRole = ? AND "
                  "(a.ValidTo IS NULL OR a.ValidTo >= "
                  "CONVERT(date, ?, 112)) ORDER BY a.IsDefault DESC,"
                  " a.AddressRef"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-O-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE WS-ADDR-ROLE TO SQLIO-PARM-VAL(3)
           MOVE WS-BUSDATE TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 300 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ADR
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               INITIALIZE WS-ADR
           END-IF.
       6750-EXIT.
           EXIT.

      *>   payment terms, the trading terms in issue and the VAT
      *>   basis.  A zero-rated export is still a stated basis -
      *>   "no VAT" is never left to be inferred (D-21).
       6760-TERMS-AND-VAT SECTION.
       6760-START.
           IF WS-O-TERMS = SPACES
               MOVE WS-CU-TERMS TO WS-O-TERMS
           END-IF
           IF WS-O-TERMS-SNAP = SPACES AND WS-O-TERMS NOT = SPACES
               MOVE SPACES TO WS-O-TERMS-SNAP
               STRING "Terms snapshotted at acceptance: "
                      FUNCTION TRIM(WS-O-TERMS)
                   DELIMITED BY SIZE INTO WS-O-TERMS-SNAP
           END-IF
           IF WS-O-TC-VERSION = SPACES
               PERFORM 6770-TERMS-VERSION-IN-ISSUE
           END-IF
           IF WS-O-VAT-BASIS = SPACES
               EVALUATE WS-O-VAT-LOC
                   WHEN "GB" MOVE "UK standard rated"
                                  TO WS-O-VAT-BASIS
                   WHEN "XI" MOVE "NI protocol goods"
                                  TO WS-O-VAT-BASIS
                   WHEN "EU" MOVE "Export zero rated"
                                  TO WS-O-VAT-BASIS
                   WHEN "ROW" MOVE "Export zero rated"
                                  TO WS-O-VAT-BASIS
                   WHEN OTHER MOVE "UK standard rated"
                                  TO WS-O-VAT-BASIS
               END-EVALUATE
           END-IF
           IF WS-O-INCO = SPACES AND WS-O-VAT-LOC NOT = "GB"
               IF WS-CU-DELIV-TERMS NOT = SPACES
                   MOVE WS-CU-DELIV-TERMS(1:3) TO WS-O-INCO
               ELSE
                   MOVE "DAP" TO WS-O-INCO
               END-IF
           END-IF.
       6760-EXIT.
           EXIT.

      *>   the version of the trading terms currently in issue.  It
      *>   is a parameter so legal can publish a new edition without
      *>   a change to this program (COM-011).
       6770-TERMS-VERSION-IN-ISSUE SECTION.
       6770-START.
           INITIALIZE WS-PARM-CA
           MOVE "GET" TO CA-FUNCTION OF WS-PARM-CA
           MOVE WS-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE WS-USER TO CA-USER OF WS-PARM-CA
           MOVE WS-BUSDATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "SO.TCS.VERSION" TO PM-PARAM-CODE
           MOVE WS-BUSDATE TO PM-AS-OF
           MOVE "STERP-TCS-2026-04" TO PM-DEFAULT-TEXT
           MOVE "Y" TO PM-USE-DEFAULT
           CALL "PARMGET" USING WS-PARM-CA
           IF PM-VALUE-TEXT NOT = SPACES
               MOVE PM-VALUE-TEXT(1:20) TO WS-O-TC-VERSION
           ELSE
               MOVE "STERP-TCS-2026-04" TO WS-O-TC-VERSION
           END-IF.
       6770-EXIT.
           EXIT.

      *>   give the caller what was used, and say so when the account
      *>   could not complete the header - the order is still raised,
      *>   but SOACCEPT will not let it bind until it is stated
       6780-PUBLISH-DEFAULTS SECTION.
       6780-START.
           MOVE WS-O-INV-CUST TO SQ-INVOICE-CUST
           MOVE WS-O-ADDR-REF TO SQ-DELIV-REF
           MOVE WS-O-ADDR-SNAP TO SQ-DELIV-SNAP
           MOVE WS-O-BILL-SNAP TO SQ-BILL-SNAP
           MOVE WS-O-VAT-BASIS TO SQ-VAT-BASIS
           MOVE WS-O-VAT-LOC TO SQ-VAT-LOCATION
           MOVE WS-O-VAT-REG TO SQ-VAT-REG-NO
           MOVE WS-O-INCO TO SQ-INCOTERMS
           MOVE WS-O-INCO-PLACE TO SQ-INCO-PLACE
           IF WS-RC NOT = 0
               GO TO 6780-EXIT
           END-IF
           IF WS-O-INV-CUST = SPACES OR WS-O-ADDR-SNAP = SPACES
              OR WS-O-TERMS = SPACES OR WS-O-TC-VERSION = SPACES
              OR WS-O-VAT-BASIS = SPACES
               MOVE 4 TO WS-RC
               MOVE "SOQ0014" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "The customer master could not complete the "
                      "order header - state the missing terms "
                      "before the order is accepted"
                   DELIMITED BY SIZE INTO WS-MSG
               ADD 1 TO WS-MSGCNT
           END-IF.
       6780-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   A new order always starts DRAFT / AWAITAPPROVAL /
      *>   UNALLOCATED / NOTELIGIBLE with no deposit received.  Those
      *>   four literals are what stops a template or a call-off from
      *>   inheriting fulfilment, billing or payment state (SO-024).
       7000-INSERT-ORDER-HDR SECTION.
       7000-START.
           IF SQ-FX-RATE = 0
               MOVE 1 TO SQ-FX-RATE
           END-IF
           COMPUTE WS-AMT1 ROUNDED = SQ-GROSS-AMOUNT / SQ-FX-RATE
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO so.SalesOrder (CompanyCode, SiteCode, "
                  "OrderNumber, OrderType, BlanketOrderNumber, "
                  "CustomerCode, InvoiceCustomerCode, "
                  "CustomerOrderRef, OrderDate, QuoteNumber, "
                  "CurrencyCode, FxRate, FxRateDate, TermsCode, "
                  "TermsSnapshot, SalespersonCode, IncotermsRule, "
                  "IncotermsNamedPlace, DeliveryAddressRef, "
                  "DeliveryAddressSnapshot, BillToAddressSnapshot, "
                  "TermsAndConditionsVersion, RetentionOfTitle, "
                  "VatTreatmentBasis, CustomerVatRegNo, VatLocation, "
                  "NetAmount, VatAmount, GrossAmount, GrossAmountGBP, "
                  "CommercialStatus, ReleaseStatus, FulfilmentStatus, "
                  "BillingStatus, DepositReceivedGBP, CorrelationId, "
                  "CreatedBy) VALUES (?, ?, ?, ?, NULLIF(?,''), ?, "
                  "NULLIF(?,''), NULLIF(?,''), CONVERT(date, ?, 112),"
                  " NULLIF(?,''), ?, CONVERT(decimal(18,8), ?), "
                  "CONVERT(date, ?, 112), NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(?,''), NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(?,''), NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(?,''), CONVERT(bit, "
                  "ISNULL(NULLIF(?,''),'1')), NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(?,''), "
                  "CONVERT(decimal(18,2), ?), "
                  "CONVERT(decimal(18,2), ?), "
                  "CONVERT(decimal(18,2), ?), "
                  "CONVERT(decimal(18,2), ?), 'DRAFT', "
                  "'AWAITAPPROVAL', 'UNALLOCATED', 'NOTELIGIBLE', 0, "
                  "NULLIF(?,''), ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 32 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SQ-SITE-CODE TO SQLIO-PARM-VAL(2)
           MOVE SQ-ORDER-NUMBER TO SQLIO-PARM-VAL(3)
           MOVE WS-O-TYPE TO SQLIO-PARM-VAL(4)
           MOVE WS-O-BLANKET TO SQLIO-PARM-VAL(5)
           MOVE WS-O-CUSTOMER TO SQLIO-PARM-VAL(6)
           MOVE WS-O-INV-CUST TO SQLIO-PARM-VAL(7)
           MOVE SQ-CUSTOMER-REF TO SQLIO-PARM-VAL(8)
           MOVE WS-BUSDATE TO SQLIO-PARM-VAL(9)
           MOVE WS-O-QUOTE-REF TO SQLIO-PARM-VAL(10)
           MOVE WS-O-CURRENCY TO SQLIO-PARM-VAL(11)
           MOVE SQ-FX-RATE TO WS-RATE-ED
           MOVE FUNCTION TRIM(WS-RATE-ED) TO SQLIO-PARM-VAL(12)
           MOVE WS-BUSDATE TO SQLIO-PARM-VAL(13)
           MOVE WS-O-TERMS TO SQLIO-PARM-VAL(14)
           MOVE WS-O-TERMS-SNAP TO SQLIO-PARM-VAL(15)
           MOVE WS-O-SALESPERSON TO SQLIO-PARM-VAL(16)
           MOVE WS-O-INCO TO SQLIO-PARM-VAL(17)
           MOVE WS-O-INCO-PLACE TO SQLIO-PARM-VAL(18)
           MOVE WS-O-ADDR-REF TO SQLIO-PARM-VAL(19)
           MOVE WS-O-ADDR-SNAP TO SQLIO-PARM-VAL(20)
           MOVE WS-O-BILL-SNAP TO SQLIO-PARM-VAL(21)
           MOVE WS-O-TC-VERSION TO SQLIO-PARM-VAL(22)
           MOVE WS-O-ROT TO SQLIO-PARM-VAL(23)
           MOVE WS-O-VAT-BASIS TO SQLIO-PARM-VAL(24)
           MOVE WS-O-VAT-REG TO SQLIO-PARM-VAL(25)
           MOVE WS-O-VAT-LOC TO SQLIO-PARM-VAL(26)
           MOVE SQ-NET-AMOUNT TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(27)
           MOVE SQ-VAT-AMOUNT TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(28)
           MOVE SQ-GROSS-AMOUNT TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(29)
           MOVE WS-AMT1 TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(30)
           MOVE WS-CORREL TO SQLIO-PARM-VAL(31)
           MOVE WS-USER TO SQLIO-PARM-VAL(32)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 32
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS
           IF WS-RC > 4
               GO TO 7000-EXIT
           END-IF
           PERFORM 7050-READ-ORDER-ID.
       7000-EXIT.
           EXIT.

       7050-READ-ORDER-ID SECTION.
       7050-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), SalesOrderId), '' "
                  "FROM so.SalesOrder WHERE CompanyCode = ? AND "
                  "OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SQ-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 7050-EXIT
           END-IF
           MOVE WS-T1 TO WS-ORDER-ID.
       7050-EXIT.
           EXIT.

       7100-INSERT-ORDER-LINES SECTION.
       7100-START.
           IF WS-RC > 4
               GO TO 7100-EXIT
           END-IF
           MOVE 0 TO SQ-LINES-CREATED
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > SQ-LINE-CNT OR WS-RC > 4
               PERFORM 7150-INSERT-ONE-OLINE
           END-PERFORM.
       7100-EXIT.
           EXIT.

       7150-INSERT-ONE-OLINE SECTION.
       7150-START.
           IF SQ-L-ITEM(WS-I) = SPACES
               MOVE "NONSTOCK" TO WS-LINE-TYPE
           ELSE
               MOVE "STOCK" TO WS-LINE-TYPE
           END-IF
           MOVE SQ-L-LINE-NUM(WS-I) TO WS-LINE-REF-N
           MOVE WS-LINE-REF-N TO WS-LINE-REF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO so.SalesOrderLine (SalesOrderId, "
                  "CompanyCode, OrderNumber, LineNum, LineRef, "
                  "ItemCode, Revision, Description, LineType, "
                  "AcceptedQty, CancelledQty, DespatchedQty, "
                  "BilledQty, ReturnedQty, UomCode, UnitPrice, "
                  "PricePerQty, PriceSource, DiscountPct1, "
                  "DiscountPct2, NetAmount, VatCode, VatAmount, "
                  "CostSnapshotGBP, CostBasis, MarginPct, Status) "
                  "VALUES (?, ?, ?, CONVERT(int, ?), ?, NULLIF(?,''),"
                  " NULLIF(?,''), ?, ?, CONVERT(decimal(19,6), ?), "
                  "0, 0, 0, 0, NULLIF(?,''), "
                  "CONVERT(decimal(19,6), ?), "
                  "CONVERT(decimal(19,6), ?), NULLIF(?,''), "
                  "CONVERT(decimal(9,4), ?), CONVERT(decimal(9,4), ?)"
                  ", CONVERT(decimal(18,2), ?), NULLIF(?,''), "
                  "CONVERT(decimal(18,2), ?), "
                  "CONVERT(decimal(19,6), ?), NULLIF(?,''), "
                  "CONVERT(decimal(9,4), ?), 'OPEN')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 22 TO SQLIO-PARM-CNT
           MOVE WS-ORDER-ID TO SQLIO-PARM-VAL(1)
           MOVE WS-CO TO SQLIO-PARM-VAL(2)
           MOVE SQ-ORDER-NUMBER TO SQLIO-PARM-VAL(3)
           MOVE SQ-L-LINE-NUM(WS-I) TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE WS-LINE-REF TO SQLIO-PARM-VAL(5)
           MOVE SQ-L-ITEM(WS-I) TO SQLIO-PARM-VAL(6)
           MOVE SQ-L-REVISION(WS-I) TO SQLIO-PARM-VAL(7)
           MOVE SQ-L-DESC(WS-I) TO SQLIO-PARM-VAL(8)
           MOVE WS-LINE-TYPE TO SQLIO-PARM-VAL(9)
           MOVE SQ-L-QTY(WS-I) TO WS-QTY-ED
           MOVE FUNCTION TRIM(WS-QTY-ED) TO SQLIO-PARM-VAL(10)
           MOVE SQ-L-UOM(WS-I) TO SQLIO-PARM-VAL(11)
           MOVE SQ-L-UNIT-PRICE(WS-I) TO WS-PRICE-ED
           MOVE FUNCTION TRIM(WS-PRICE-ED) TO SQLIO-PARM-VAL(12)
           MOVE SQ-L-PRICE-PER(WS-I) TO WS-PRICE-ED
           MOVE FUNCTION TRIM(WS-PRICE-ED) TO SQLIO-PARM-VAL(13)
           MOVE SQ-L-PRICE-SRC(WS-I) TO SQLIO-PARM-VAL(14)
           MOVE SQ-L-DISC1(WS-I) TO WS-PCT-ED
           MOVE FUNCTION TRIM(WS-PCT-ED) TO SQLIO-PARM-VAL(15)
           MOVE SQ-L-DISC2(WS-I) TO WS-PCT-ED
           MOVE FUNCTION TRIM(WS-PCT-ED) TO SQLIO-PARM-VAL(16)
           MOVE SQ-L-NET(WS-I) TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(17)
           MOVE SQ-L-VAT-CODE(WS-I) TO SQLIO-PARM-VAL(18)
           MOVE SQ-L-VAT-AMT(WS-I) TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(19)
           MOVE SQ-L-COST-GBP(WS-I) TO WS-PRICE-ED
           MOVE FUNCTION TRIM(WS-PRICE-ED) TO SQLIO-PARM-VAL(20)
           MOVE SQ-L-COST-BASIS(WS-I) TO SQLIO-PARM-VAL(21)
           MOVE SQ-L-MARGIN-PCT(WS-I) TO WS-PCT-ED
           MOVE FUNCTION TRIM(WS-PCT-ED) TO SQLIO-PARM-VAL(22)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 22
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS
           IF WS-RC < 8
               ADD 1 TO SQ-LINES-CREATED
           END-IF.
       7150-EXIT.
           EXIT.

      *>   the quote's own record of what it became
       7200-MARK-CONVERTED SECTION.
       7200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.Quote SET Status = 'CONVERTED', "
                  "ConvertedOrderNumber = ?, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE CompanyCode = ? "
                  "AND QuoteNumber = ? AND Version = ? AND "
                  "ConvertedOrderNumber IS NULL"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE SQ-ORDER-NUMBER TO SQLIO-PARM-VAL(1)
           MOVE WS-USER TO SQLIO-PARM-VAL(2)
           MOVE WS-CO TO SQLIO-PARM-VAL(3)
           MOVE SQ-QUOTE-NUMBER TO SQLIO-PARM-VAL(4)
           MOVE SQ-VERSION TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS
           IF WS-RC < 8
               MOVE "CONVERTED" TO SQ-STATUS
           END-IF.
       7200-EXIT.
           EXIT.

      *>   COM-013 - who changed what, when and with what authority
       7500-AUDIT-THE-CHANGE SECTION.
       7500-START.
           IF WS-RC > 4
               GO TO 7500-EXIT
           END-IF
           INITIALIZE WS-AUD-CA
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUD-CA
           MOVE WS-CO TO CA-COMPANY OF WS-AUD-CA
           MOVE WS-SITE TO CA-SITE OF WS-AUD-CA
           MOVE WS-USER TO CA-USER OF WS-AUD-CA
           MOVE WS-BUSDATE TO CA-BUSINESS-DATE OF WS-AUD-CA
           MOVE WS-CORREL TO CA-CORRELATION-ID OF WS-AUD-CA
           MOVE "so.Quote" TO AU-ENTITY-TYPE
           MOVE SPACES TO AU-ENTITY-KEY
           MOVE SQ-VERSION TO WS-VER-ED
           STRING FUNCTION TRIM(WS-CO) "/"
                  FUNCTION TRIM(SQ-QUOTE-NUMBER) "/V"
                  FUNCTION TRIM(WS-VER-ED)
               DELIMITED BY SIZE INTO AU-ENTITY-KEY
           MOVE WS-FUNC TO AU-ACTION
           MOVE "Status" TO AU-FIELD-NAME
           MOVE SQ-PREV-STATUS TO AU-OLD-VALUE
           MOVE SQ-STATUS TO AU-NEW-VALUE
           MOVE SPACES TO AU-REASON
           STRING "SOQUOTE " FUNCTION TRIM(WS-FUNC) " "
                  FUNCTION TRIM(SQ-ORDER-NUMBER)
               DELIMITED BY SIZE INTO AU-REASON
           MOVE SQ-ACCEPT-EVIDENCE TO AU-AUTHORITY-REF
           MOVE "SOQUOTE" TO AU-SOURCE
           MOVE "N" TO AU-SENSITIVE
           CALL "AUDITLOG" USING WS-AUD-CA.
       7500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-BEGIN-TRAN SECTION.
       8000-START.
           IF WS-RC > 4 OR WS-TRAN-OPEN = "Y"
               GO TO 8000-EXIT
           END-IF
           MOVE 0 TO SQLIO-PARM-CNT SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
           ELSE
               MOVE "Y" TO WS-TRAN-OPEN
           END-IF.
       8000-EXIT.
           EXIT.

       8100-COMMIT-TRAN SECTION.
       8100-START.
           IF WS-TRAN-OPEN NOT = "Y"
               GO TO 8100-EXIT
           END-IF
           MOVE 0 TO SQLIO-PARM-CNT SQLIO-COL-CNT
           MOVE "COMT" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
           END-IF
           MOVE "N" TO WS-TRAN-OPEN.
       8100-EXIT.
           EXIT.

      *>   run the statement already built in SQLIO-STMT
       8200-EXEC-NO-ROWS SECTION.
       8200-START.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 8200-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO.
       8200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE 8 TO WS-RC
           ADD 1 TO WS-MSGCNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE 8 TO WS-RC
           MOVE "SOQ0003" TO WS-REASON
           MOVE SPACES TO WS-MSG
           STRING "SOQUOTE does not support function ["
                  FUNCTION TRIM(WS-FUNC) "]"
               DELIMITED BY SIZE INTO WS-MSG
           ADD 1 TO WS-MSGCNT.
       9100-EXIT.
           EXIT.

       9200-BAD-STATUS SECTION.
       9200-START.
           MOVE "SOQ0005" TO WS-REASON
           MOVE SQ-VERSION TO WS-VER-ED
           MOVE SPACES TO WS-MSG
           STRING "Quote " FUNCTION TRIM(SQ-QUOTE-NUMBER)
                  " version " FUNCTION TRIM(WS-VER-ED) " is "
                  FUNCTION TRIM(SQ-STATUS)
                  " - that status does not allow "
                  FUNCTION TRIM(WS-FUNC)
               DELIMITED BY SIZE INTO WS-MSG
           PERFORM 9000-REJECT.
       9200-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE 12 TO WS-RC
           MOVE "SQL0001" TO WS-REASON
           MOVE SQLIO-ERRMSG TO WS-MSG
           ADD 1 TO WS-MSGCNT.
       9300-EXIT.
           EXIT.

      *>   SO-001 - a repeated conversion reports the existing order
      *>   with 04.  It is never a refusal and never a second demand.
       9400-ALREADY-CONVERTED SECTION.
       9400-START.
           MOVE 4 TO WS-RC
           MOVE "Y" TO SQ-ALREADY-DONE
           MOVE "SOQ0011" TO WS-REASON
           MOVE "CONVERTED" TO SQ-STATUS
           MOVE SPACES TO WS-MSG
           STRING "Quote " FUNCTION TRIM(SQ-QUOTE-NUMBER)
                  " has already been converted to order "
                  FUNCTION TRIM(SQ-ORDER-NUMBER)
                  " - the existing order is returned"
               DELIMITED BY SIZE INTO WS-MSG
           ADD 1 TO WS-MSGCNT.
       9400-EXIT.
           EXIT.

       9500-ROLLBACK SECTION.
       9500-START.
           IF WS-TRAN-OPEN = "Y"
               MOVE 0 TO SQLIO-PARM-CNT SQLIO-COL-CNT
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-TWO
               MOVE "N" TO WS-TRAN-OPEN
           END-IF
           IF WS-IDEM-OPEN = "Y"
               MOVE "END" TO CA-FUNCTION OF WS-IDEM-CA
               MOVE "FAILED" TO ID-STATUS
               MOVE WS-MSG TO ID-RESULT-MESSAGE
               CALL "IDEMPOT" USING WS-IDEM-CA
               MOVE "N" TO WS-IDEM-OPEN
           END-IF.
       9500-EXIT.
           EXIT.

       9900-SET-RESULT SECTION.
       9900-START.
           MOVE WS-RC TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE WS-REASON TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE WS-MSGCNT TO CA-MSG-COUNT OF DFHCOMMAREA.
       9900-EXIT.
           EXIT.
