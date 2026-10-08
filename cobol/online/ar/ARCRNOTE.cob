      *>================================================================
      *> PROGRAM-ID : ARCRNOTE
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : SALES CREDIT NOTES.  A CREDIT IDENTIFIES THE
      *>              ORIGINAL INVOICE AND LINE WHERE ONE EXISTS, ITS
      *>              REASON, THE REMAINING CREDITABLE AMOUNT AND THE
      *>              APPROVAL.  IT NEITHER EDITS THE ORIGINAL
      *>              INVOICE NOR RETURNS STOCK - A PHYSICAL RETURN
      *>              IS A SEPARATE CONTROLLED EVENT IN SALES.
      *> CALLED BY  : CREDIT NOTE ENTRY, RETURNS PROCESSING (SO),
      *>              .NET CLIENT
      *> INTERFACE  : CALL "ARCRNOTE" USING WS-CRNOTE-CA
      *>              COPYBOOK ARCRNOTECA
      *> CALLS      : SQLIO, PERMCHK, PERIODCK, NUMGEN, IDEMPOT,
      *>              ARVATWRT, ARPOSTGL
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-05-02 RJH  ORIGINAL MVS SALES LEDGER CREDIT NOTES
      *> 1997-09-30 DJM  CR0860 OVER CREDIT CHECK AGAINST PRIOR
      *>                 CREDITS AFTER THE PENNINE QUERY
      *> 2026-10-05 STERP REMAINING CREDITABLE PER LINE
      *>================================================================
      *> REQ: AR-006, AR-012, AR-020, AR-023
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARCRNOTE.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 02/05/1991.
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
       COPY ARVATWRTCA REPLACING ==DFHCOMMAREA== BY ==WS-VATW-CA==.
       COPY ARPOSTGLCA REPLACING ==DFHCOMMAREA== BY ==WS-POSTGL-CA==.
       01  WS-ORIG.
           05  WS-O-ID                 PIC X(24).
           05  WS-O-CUSTOMER           PIC X(20).
           05  WS-O-NAME               PIC X(120).
           05  WS-O-CURRENCY           PIC X(3).
           05  WS-O-RATE               PIC X(24).
           05  WS-O-NET                PIC X(24).
           05  WS-O-VAT                PIC X(24).
           05  WS-O-GROSS              PIC X(24).
           05  WS-O-PRIOR              PIC X(24).
           05  WS-O-VATNO              PIC X(15).
           05  WS-O-CONTROL            PIC X(12).
           05  WS-O-TAXPOINT           PIC X(10).
       01  WS-LINE-ROW.
           05  WS-L-NET                PIC X(24).
           05  WS-L-VATCODE            PIC X(4).
           05  WS-L-RATE               PIC X(24).
           05  WS-L-CREDITED           PIC X(24).
           05  WS-L-DESC               PIC X(60).
           05  WS-L-ACCOUNT            PIC X(12).
           05  WS-L-CC                 PIC X(20).
       01  WS-ID-ROW.
           05  WS-NEW-ID               PIC X(24).
           05  WS-ID2                  PIC X(24).
       01  WS-SW.
           05  WS-TRAN-OPEN            PIC X VALUE "N".
           05  WS-LEG                  PIC S9(4) VALUE 0.
           05  WS-FOUND                PIC X VALUE "N".
           05  WS-VAT-IX               PIC S9(4) VALUE 0.
       01  WS-VATS.
           05  WS-VAT-CNT              PIC S9(4) VALUE 0.
           05  WS-VAT-TAB OCCURS 6 TIMES.
               10  WS-V-CODE           PIC X(4).
               10  WS-V-RATE           PIC S9(3)V9(4).
               10  WS-V-NET            PIC S9(11)V99.
               10  WS-V-VAT            PIC S9(11)V99.
       LINKAGE SECTION.
       COPY ARCRNOTECA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF AR-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0300-READ-ORIGINAL
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0400-PRICE-LINES
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0500-OVER-CREDIT-CHECK
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE AR-FUNC
               WHEN "CHECK"    PERFORM 0600-REPORT-REMAINING
               WHEN "POST"     PERFORM 1000-POST-CREDIT
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
           INITIALIZE WS-ORIG
           INITIALIZE WS-SW
           INITIALIZE WS-VATS
           MOVE CA-FUNCTION OF DFHCOMMAREA TO AR-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO AR-CO
           MOVE CA-SITE OF DFHCOMMAREA TO AR-SITE
           MOVE CA-USER OF DFHCOMMAREA TO AR-USERID
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO AR-BUSDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO AR-CORREL
           MOVE "ARCRNOTE" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO AR-RC AR-MSG-CNT
           MOVE SPACES TO AR-REASON AR-MSG
           MOVE "AR-006" TO AR-REQ-REF
           MOVE 0 TO AN-CREDIT-ID AN-OPEN-ITEM-ID
           MOVE SPACES TO AN-JOURNAL-NO
           MOVE 0 TO AN-NET AN-VAT AN-GROSS
           MOVE 0 TO AN-NET-GBP AN-VAT-GBP AN-GROSS-GBP
           IF AN-DOC-DATE = 0
               MOVE AR-BUSDATE TO AN-DOC-DATE
           END-IF
           IF AN-TAX-POINT = 0
               MOVE AN-DOC-DATE TO AN-TAX-POINT
           END-IF
           IF AN-POSTING-DATE = 0
               MOVE AN-DOC-DATE TO AN-POSTING-DATE
           END-IF
           IF AN-CREDIT-TYPE = SPACES
               MOVE "VALUE" TO AN-CREDIT-TYPE
           END-IF
           IF AN-RETURN-STOCK = SPACE
              OR AN-RETURN-STOCK = LOW-VALUE
               MOVE "N" TO AN-RETURN-STOCK
           END-IF.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF AR-CO = SPACES
               MOVE "ARC0001" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A company code is required" DELIMITED BY SIZE
                   INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AN-REASON = SPACES
               MOVE "ARC0002" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A credit note needs a stated reason"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AN-RETURN-STOCK = "Y"
               MOVE "ARC0003" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A financial credit does not return stock; "
                      "raise a return authorisation in Sales"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AN-GOODWILL
               IF AN-ORIG-INVOICE NOT = SPACES
                   MOVE 4 TO AR-RC
                   MOVE "ARC0004" TO AR-REASON
                   MOVE SPACES TO AR-MSG
                   STRING "A goodwill credit normally has no original"
                          " invoice; the link has been kept"
                       DELIMITED BY SIZE INTO AR-MSG
                   ADD 1 TO AR-MSG-CNT
               END-IF
           ELSE
               IF AN-ORIG-INVOICE = SPACES
                   MOVE "ARC0005" TO AR-REASON
                   MOVE SPACES TO AR-MSG
                   STRING "The original invoice must be identified "
                          "where one exists"
                       DELIMITED BY SIZE INTO AR-MSG
                   PERFORM 9000-REJECT
               END-IF
           END-IF
           IF AN-LINE-CNT < 1 OR AN-LINE-CNT > 20
               MOVE "ARC0006" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A credit note needs between one and twenty "
                      "lines" DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0300-READ-ORIGINAL SECTION.
       0300-START.
           IF AN-ORIG-INVOICE = SPACES
               MOVE 0 TO AN-ORIG-INVOICE-ID
               MOVE 0 TO AN-CREDITABLE-REM
               GO TO 0300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), i.InvoiceId), "
                  "i.CustomerCode, ISNULL(i.BillToName,''), "
                  "i.CurrencyCode, CONVERT(varchar(24), i.FxRate), "
                  "CONVERT(varchar(24), i.NetAmountGBP), "
                  "CONVERT(varchar(24), i.VatAmountGBP), "
                  "CONVERT(varchar(24), i.GrossAmountGBP), "
                  "CONVERT(varchar(24), ISNULL((SELECT "
                  "SUM(c.GrossAmountGBP) FROM ar.SalesInvoice c "
                  "WHERE c.CompanyCode = i.CompanyCode AND "
                  "c.DocumentType = 'CREDITNOTE' AND "
                  "c.OriginalInvoiceNumber = i.InvoiceNumber AND "
                  "c.Status = 'POSTED'),0)), "
                  "ISNULL(i.CustomerVatRegNo,''), "
                  "ISNULL((SELECT k.ArControlAccount FROM ar.Customer"
                  " k WHERE k.CompanyCode = i.CompanyCode AND "
                  "k.CustomerCode = i.CustomerCode),''), "
                  "CONVERT(char(8), i.TaxPointDate, 112) "
                  "FROM ar.SalesInvoice i WHERE i.CompanyCode = ? "
                  "AND i.InvoiceNumber = ? AND i.DocumentType = "
                  "'INVOICE' AND i.Status = 'POSTED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE AN-ORIG-INVOICE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 12 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 120 TO SQLIO-COL-WIDTH(3)
           MOVE 3 TO SQLIO-COL-WIDTH(4)
           MOVE 24 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 24 TO SQLIO-COL-WIDTH(8)
           MOVE 24 TO SQLIO-COL-WIDTH(9)
           MOVE 15 TO SQLIO-COL-WIDTH(10)
           MOVE 12 TO SQLIO-COL-WIDTH(11)
           MOVE 10 TO SQLIO-COL-WIDTH(12)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ORIG
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "ARC0010" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Posted invoice "
                      FUNCTION TRIM(AN-ORIG-INVOICE)
                      " is not held for this company"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 0300-EXIT
           END-IF
           COMPUTE AN-ORIG-INVOICE-ID = FUNCTION NUMVAL(WS-O-ID)
           MOVE WS-O-CUSTOMER TO AN-CUSTOMER
           COMPUTE AN-ORIG-NET-GBP = FUNCTION NUMVAL(WS-O-NET)
           COMPUTE AN-ORIG-VAT-GBP = FUNCTION NUMVAL(WS-O-VAT)
           COMPUTE AN-ORIG-GROSS-GBP = FUNCTION NUMVAL(WS-O-GROSS)
           COMPUTE AN-PRIOR-CREDIT = FUNCTION NUMVAL(WS-O-PRIOR)
           COMPUTE AN-CREDITABLE-REM =
               AN-ORIG-GROSS-GBP - AN-PRIOR-CREDIT
           IF AN-CURRENCY = SPACES
               MOVE WS-O-CURRENCY TO AN-CURRENCY
           END-IF
           IF AN-FX-RATE = 0
               COMPUTE AN-FX-RATE = FUNCTION NUMVAL(WS-O-RATE)
           END-IF.
       0300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-PRICE-LINES SECTION.
       0400-START.
           IF AN-CURRENCY = SPACES
               MOVE "GBP" TO AN-CURRENCY
           END-IF
           IF AN-FX-RATE = 0
               MOVE 1 TO AN-FX-RATE
           END-IF
           MOVE 0 TO AN-NET AN-VAT
           MOVE 0 TO AN-NET-GBP AN-VAT-GBP
           MOVE 0 TO WS-VAT-CNT
           PERFORM VARYING AR-I FROM 1 BY 1
               UNTIL AR-I > AN-LINE-CNT OR AR-RC-BAD
               PERFORM 0420-ONE-LINE
           END-PERFORM
           IF AR-RC-BAD
               GO TO 0400-EXIT
           END-IF
           COMPUTE AN-GROSS = AN-NET + AN-VAT
           IF AN-CURRENCY = "GBP"
               MOVE AN-NET TO AN-NET-GBP
               MOVE AN-VAT TO AN-VAT-GBP
           END-IF
           COMPUTE AN-GROSS-GBP = AN-NET-GBP + AN-VAT-GBP.
       0400-EXIT.
           EXIT.

       0420-ONE-LINE SECTION.
       0420-START.
           MOVE SPACES TO AN-L-MESSAGE(AR-I)
           IF AN-L-DESC(AR-I) = SPACES
               MOVE "Credit" TO AN-L-DESC(AR-I)
           END-IF
           IF AN-L-NET(AR-I) <= 0
               MOVE "ARC0020" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Every credit line needs a positive net amount"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0420-EXIT
           END-IF
           PERFORM 0440-LINE-AGAINST-ORIGINAL
           IF AR-RC-BAD
               GO TO 0420-EXIT
           END-IF
           PERFORM 0460-LINE-VAT
           IF AR-RC-BAD
               GO TO 0420-EXIT
           END-IF
           ADD AN-L-NET(AR-I) TO AN-NET
           ADD AN-L-VAT-AMT(AR-I) TO AN-VAT
           IF AN-CURRENCY NOT = "GBP" AND AN-FX-RATE > 0
               COMPUTE AR-AMT2 ROUNDED = AN-L-NET(AR-I) / AN-FX-RATE
               COMPUTE AR-AMT3 ROUNDED =
                   AN-L-VAT-AMT(AR-I) / AN-FX-RATE
               ADD AR-AMT2 TO AN-NET-GBP
               ADD AR-AMT3 TO AN-VAT-GBP
           ELSE
               MOVE AN-L-NET(AR-I) TO AR-AMT2
               MOVE AN-L-VAT-AMT(AR-I) TO AR-AMT3
           END-IF
           PERFORM 0480-ACCUMULATE-VAT.
       0420-EXIT.
           EXIT.

      *>   the original line sets the remaining creditable amount
       0440-LINE-AGAINST-ORIGINAL SECTION.
       0440-START.
           MOVE 0 TO AN-L-ORIG-NET(AR-I)
           MOVE 0 TO AN-L-ORIG-CREDIT(AR-I)
           MOVE 0 TO AN-L-REMAINING(AR-I)
           IF AN-ORIG-INVOICE-ID = 0 OR AN-L-ORIG-LINE(AR-I) = 0
               GO TO 0440-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), l.NetAmount), "
                  "l.VatCode, CONVERT(varchar(24), l.VatRatePct), "
                  "CONVERT(varchar(24), ISNULL((SELECT "
                  "SUM(cl.NetAmount) FROM ar.SalesInvoiceLine cl "
                  "JOIN ar.SalesInvoice ch ON ch.InvoiceId = "
                  "cl.InvoiceId WHERE ch.CompanyCode = ? AND "
                  "ch.DocumentType = 'CREDITNOTE' AND "
                  "ch.OriginalInvoiceNumber = ? AND ch.Status = "
                  "'POSTED' AND cl.SalesOrderLine = l.LineNum),0)), "
                  "l.Description, ISNULL(l.RevenueAccountCode,''), "
                  "ISNULL(l.DimCostCentre,'') "
                  "FROM ar.SalesInvoiceLine l WHERE l.InvoiceId = ? "
                  "AND l.LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE AN-ORIG-INVOICE TO SQLIO-PARM-VAL(2)
           MOVE AN-ORIG-INVOICE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE AN-L-ORIG-LINE(AR-I) TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(4)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 4
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE 7 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 4 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 60 TO SQLIO-COL-WIDTH(5)
           MOVE 12 TO SQLIO-COL-WIDTH(6)
           MOVE 20 TO SQLIO-COL-WIDTH(7)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0440-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 8 TO AR-RC
               MOVE "ARC0021" TO AR-REASON
               MOVE AN-L-ORIG-LINE(AR-I) TO AR-INT-ED
               MOVE SPACES TO AR-MSG
               STRING "Line " FUNCTION TRIM(AR-INT-ED)
                      " does not exist on invoice "
                      FUNCTION TRIM(AN-ORIG-INVOICE)
                   DELIMITED BY SIZE INTO AR-MSG
               MOVE AR-MSG TO AN-L-MESSAGE(AR-I)
               ADD 1 TO AR-MSG-CNT
               GO TO 0440-EXIT
           END-IF
           COMPUTE AN-L-ORIG-NET(AR-I) = FUNCTION NUMVAL(WS-L-NET)
           COMPUTE AN-L-ORIG-CREDIT(AR-I) =
               FUNCTION NUMVAL(WS-L-CREDITED)
           COMPUTE AN-L-REMAINING(AR-I) =
               AN-L-ORIG-NET(AR-I) - AN-L-ORIG-CREDIT(AR-I)
           IF AN-L-VAT-CODE(AR-I) = SPACES
               MOVE WS-L-VATCODE TO AN-L-VAT-CODE(AR-I)
           END-IF
           IF AN-L-REV-ACCOUNT(AR-I) = SPACES
               MOVE WS-L-ACCOUNT TO AN-L-REV-ACCOUNT(AR-I)
           END-IF
           IF AN-L-NET(AR-I) > AN-L-REMAINING(AR-I)
               MOVE 8 TO AR-RC
               MOVE "ARC0022" TO AR-REASON
               MOVE AN-L-REMAINING(AR-I) TO AR-MONEY-UK
               MOVE SPACES TO AR-MSG
               STRING "The credit exceeds the remaining creditable "
                      "amount on that line of "
                      FUNCTION TRIM(AR-MONEY-UK)
                   DELIMITED BY SIZE INTO AR-MSG
               MOVE AR-MSG TO AN-L-MESSAGE(AR-I)
               ADD 1 TO AR-MSG-CNT
           END-IF.
       0440-EXIT.
           EXIT.

       0460-LINE-VAT SECTION.
       0460-START.
           INITIALIZE WS-VATW-CA
           MOVE "LOOKUP" TO CA-FUNCTION OF WS-VATW-CA
           MOVE AR-CO TO CA-COMPANY OF WS-VATW-CA
           MOVE AR-USERID TO CA-USER OF WS-VATW-CA
           MOVE AN-TAX-POINT TO CA-BUSINESS-DATE OF WS-VATW-CA
           MOVE AN-L-VAT-CODE(AR-I) TO VW-VAT-CODE
           MOVE AN-TAX-POINT TO VW-TAX-POINT
           MOVE "OUTPUT" TO VW-DIRECTION
           CALL "ARVATWRT" USING WS-VATW-CA
           IF CA-RETURN-CODE OF WS-VATW-CA > 6
               MOVE 8 TO AR-RC
               MOVE CA-REASON-CODE OF WS-VATW-CA TO AR-REASON
               MOVE CA-MESSAGE OF WS-VATW-CA TO AR-MSG
               MOVE AR-MSG TO AN-L-MESSAGE(AR-I)
               ADD 1 TO AR-MSG-CNT
               GO TO 0460-EXIT
           END-IF
           MOVE VW-RATE-PCT TO AN-L-VAT-RATE(AR-I)
           COMPUTE AR-AMT1 ROUNDED =
               AN-L-NET(AR-I) * VW-RATE-PCT / 100
           MOVE AR-AMT1 TO AN-L-VAT-AMT(AR-I).
       0460-EXIT.
           EXIT.

       0480-ACCUMULATE-VAT SECTION.
       0480-START.
           MOVE "N" TO WS-FOUND
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > WS-VAT-CNT
               IF WS-V-CODE(AR-J) = AN-L-VAT-CODE(AR-I)
                   MOVE "Y" TO WS-FOUND
                   MOVE AR-J TO WS-VAT-IX
               END-IF
           END-PERFORM
           IF WS-FOUND = "N"
               IF WS-VAT-CNT >= 6
                   GO TO 0480-EXIT
               END-IF
               ADD 1 TO WS-VAT-CNT
               MOVE WS-VAT-CNT TO WS-VAT-IX
               MOVE AN-L-VAT-CODE(AR-I) TO WS-V-CODE(WS-VAT-IX)
               MOVE AN-L-VAT-RATE(AR-I) TO WS-V-RATE(WS-VAT-IX)
               MOVE 0 TO WS-V-NET(WS-VAT-IX)
               MOVE 0 TO WS-V-VAT(WS-VAT-IX)
           END-IF
           ADD AR-AMT2 TO WS-V-NET(WS-VAT-IX)
           ADD AR-AMT3 TO WS-V-VAT(WS-VAT-IX).
       0480-EXIT.
           EXIT.

      *>   over credit control at document level (CR0860)
       0500-OVER-CREDIT-CHECK SECTION.
       0500-START.
           IF AN-ORIG-INVOICE-ID = 0
               GO TO 0500-EXIT
           END-IF
           IF AN-GROSS-GBP > AN-CREDITABLE-REM
               MOVE 8 TO AR-RC
               MOVE "ARC0030" TO AR-REASON
               MOVE AN-CREDITABLE-REM TO AR-MONEY-UK
               MOVE AN-GROSS-GBP TO AR-MONEY-ED
               MOVE SPACES TO AR-MSG
               STRING "Credit of GBP "
                      FUNCTION TRIM(AR-MONEY-ED)
                      " exceeds the remaining creditable amount of "
                      "GBP " FUNCTION TRIM(AR-MONEY-UK)
                      " on invoice " FUNCTION TRIM(AN-ORIG-INVOICE)
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       0500-EXIT.
           EXIT.

       0600-REPORT-REMAINING SECTION.
       0600-START.
           COMPUTE AR-AMT1 = AN-CREDITABLE-REM - AN-GROSS-GBP
           MOVE AR-AMT1 TO AR-MONEY-UK
           MOVE SPACES TO AR-MSG
           STRING "Remaining creditable on "
                  FUNCTION TRIM(AN-ORIG-INVOICE)
                  " after this credit: GBP "
                  FUNCTION TRIM(AR-MONEY-UK)
               DELIMITED BY SIZE INTO AR-MSG.
       0600-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-POST-CREDIT SECTION.
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
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "Y" TO WS-TRAN-OPEN
           PERFORM 1300-WRITE-HEADER
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           PERFORM 1400-WRITE-LINES
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           PERFORM 1600-POST-JOURNAL
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           PERFORM 1700-WRITE-VAT-RECORDS
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           PERFORM 1800-WRITE-OPEN-ITEM
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           PERFORM 1900-MARK-POSTED
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           MOVE "COMT" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "N" TO WS-TRAN-OPEN
           PERFORM 1950-IDEMPOTENT-END
           MOVE AN-GROSS TO AR-MONEY-UK
           MOVE SPACES TO AR-MSG
           STRING "Credit note " FUNCTION TRIM(AN-CREDIT-NO)
                  " posted, " FUNCTION TRIM(AN-CURRENCY) " "
                  FUNCTION TRIM(AR-MONEY-UK)
                  "; the original invoice is unchanged"
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
           MOVE AN-POSTING-DATE TO CA-BUSINESS-DATE OF WS-PERM-CA
           MOVE "AR.CREDITNOTE.ENTER" TO PK-PERMISSION
           CALL "PERMCHK" USING WS-PERM-CA
           IF CA-RETURN-CODE OF WS-PERM-CA > 4
               MOVE 6 TO AR-RC
               MOVE "ARC0040" TO AR-REASON
               MOVE CA-MESSAGE OF WS-PERM-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 1050-EXIT
           END-IF
      *>   an approved credit needs an independent approver above the
      *>   clerk band (D-07 matrix, AR.CREDITNOTE.APPROVE)
           IF AN-GROSS-GBP > 1000 AND AN-APPROVAL-ID = 0
              AND AN-APPROVED-BY = SPACES
               MOVE 6 TO AR-RC
               MOVE "ARC0041" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A credit note above GBP 1,000.00 needs an "
                      "independent approval before posting"
                   DELIMITED BY SIZE INTO AR-MSG
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
           MOVE AN-POSTING-DATE TO CA-BUSINESS-DATE OF WS-PERIOD-CA
           MOVE AN-POSTING-DATE TO PC-POSTING-DATE
           MOVE AN-DOC-DATE TO PC-DOCUMENT-DATE
           MOVE AN-TAX-POINT TO PC-TAX-POINT
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
               MOVE "ARC0042" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A correlation id is required to post a credit"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 1150-EXIT
           END-IF
           INITIALIZE WS-IDEM-CA
           MOVE "BEGIN" TO CA-FUNCTION OF WS-IDEM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-IDEM-CA
           MOVE AR-USERID TO CA-USER OF WS-IDEM-CA
           MOVE AR-CORREL TO CA-CORRELATION-ID OF WS-IDEM-CA
           MOVE AN-POSTING-DATE TO CA-BUSINESS-DATE OF WS-IDEM-CA
           MOVE "ar.SalesCredit.Post" TO ID-OPERATION
           CALL "IDEMPOT" USING WS-IDEM-CA
           EVALUATE CA-RETURN-CODE OF WS-IDEM-CA
               WHEN 0      CONTINUE
               WHEN 4      MOVE ID-RESULT-REF TO AN-CREDIT-NO
                           MOVE 4 TO AR-RC
                           MOVE "ARC0043" TO AR-REASON
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
           IF AN-CREDIT-NO NOT = SPACES
               GO TO 1200-EXIT
           END-IF
           INITIALIZE WS-NUM-CA
           MOVE "NEXT" TO CA-FUNCTION OF WS-NUM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-NUM-CA
           MOVE AR-USERID TO CA-USER OF WS-NUM-CA
           MOVE AR-CORREL TO CA-CORRELATION-ID OF WS-NUM-CA
           MOVE AN-DOC-DATE TO CA-BUSINESS-DATE OF WS-NUM-CA
           MOVE "SCRN" TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUM-CA
           IF CA-RETURN-CODE OF WS-NUM-CA > 4
               MOVE CA-RETURN-CODE OF WS-NUM-CA TO AR-RC
               MOVE CA-REASON-CODE OF WS-NUM-CA TO AR-REASON
               MOVE CA-MESSAGE OF WS-NUM-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 1200-EXIT
           END-IF
           MOVE NG-DOCUMENT-NUMBER TO AN-CREDIT-NO.
       1200-EXIT.
           EXIT.

       1300-WRITE-HEADER SECTION.
       1300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT ar.SalesInvoice "
                  "(CompanyCode, DocumentType, InvoiceNumber, "
                  "CustomerCode, DocumentDate, TaxPointDate, "
                  "PostingDate, DueDate, FiscalYear, PeriodNo, "
                  "CurrencyCode, FxRate, NetAmount, VatAmount, "
                  "GrossAmount, NetAmountGBP, VatAmountGBP, "
                  "GrossAmountGBP, BillToName, CustomerVatRegNo, "
                  "OriginalInvoiceNumber, CreditReason, "
                  "CreditableRemainingGBP, Status, ApprovedBy, "
                  "CorrelationId, CreatedBy) VALUES (?, 'CREDITNOTE',"
                  " ?, ?, ?, ?, ?, ?, NULLIF(?,0), NULLIF(?,0), ?, ?,"
                  " ?, ?, ?, ?, ?, ?, NULLIF(?,''), NULLIF(?,''), "
                  "NULLIF(?,''), ?, ?, 'DRAFT', NULLIF(?,''), ?, ?); "
                  "SELECT CONVERT(varchar(24), SCOPE_IDENTITY()), ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 25 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE AN-CREDIT-NO TO SQLIO-PARM-VAL(2)
           MOVE AN-CUSTOMER TO SQLIO-PARM-VAL(3)
           MOVE AN-DOC-DATE TO SQLIO-PARM-VAL(4)
           MOVE AN-TAX-POINT TO SQLIO-PARM-VAL(5)
           MOVE AN-POSTING-DATE TO SQLIO-PARM-VAL(6)
           MOVE AN-DOC-DATE TO SQLIO-PARM-VAL(7)
           MOVE PC-FISCAL-YEAR TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(8)
           MOVE PC-PERIOD-NO TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(9)
           MOVE AN-CURRENCY TO SQLIO-PARM-VAL(10)
           MOVE AN-FX-RATE TO AR-RATE-ED
           MOVE FUNCTION TRIM(AR-RATE-ED) TO SQLIO-PARM-VAL(11)
           MOVE AN-NET TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(12)
           MOVE AN-VAT TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(13)
           MOVE AN-GROSS TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(14)
           MOVE AN-NET-GBP TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(15)
           MOVE AN-VAT-GBP TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(16)
           MOVE AN-GROSS-GBP TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(17)
           MOVE WS-O-NAME TO SQLIO-PARM-VAL(18)
           MOVE WS-O-VATNO TO SQLIO-PARM-VAL(19)
           MOVE AN-ORIG-INVOICE TO SQLIO-PARM-VAL(20)
           MOVE AN-REASON TO SQLIO-PARM-VAL(21)
           COMPUTE AR-AMT1 = AN-CREDITABLE-REM - AN-GROSS-GBP
           MOVE AR-AMT1 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(22)
           MOVE AN-APPROVED-BY TO SQLIO-PARM-VAL(23)
           MOVE AR-CORREL TO SQLIO-PARM-VAL(24)
           MOVE AR-USERID TO SQLIO-PARM-VAL(25)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 25
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
           COMPUTE AN-CREDIT-ID = FUNCTION NUMVAL(WS-NEW-ID).
       1300-EXIT.
           EXIT.

      *>   SalesOrderLine carries the original invoice line number so
      *>   the over credit check can find prior credits per line
       1400-WRITE-LINES SECTION.
       1400-START.
           PERFORM VARYING AR-I FROM 1 BY 1
               UNTIL AR-I > AN-LINE-CNT OR AR-RC-BAD
               MOVE SPACES TO SQLIO-STMT
               STRING "INSERT ar.SalesInvoiceLine (InvoiceId, "
                      "LineNum, ItemCode, Description, Quantity, "
                      "UnitPrice, PricePerQty, NetAmount, VatCode, "
                      "VatRatePct, VatAmount, NetAmountGBP, "
                      "VatAmountGBP, RevenueAccountCode, "
                      "SalesOrderLine) VALUES (?, ?, NULLIF(?,''), ?,"
                      " ?, ?, 1, ?, ?, ?, ?, ?, ?, NULLIF(?,''), "
                      "NULLIF(?,0))"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 14 TO SQLIO-PARM-CNT
               MOVE AN-CREDIT-ID TO AR-INT-ED
               MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(1)
               MOVE AR-I TO AR-INT-ED
               MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(2)
               MOVE AN-L-ITEM(AR-I) TO SQLIO-PARM-VAL(3)
               MOVE AN-L-DESC(AR-I) TO SQLIO-PARM-VAL(4)
               IF AN-L-QTY(AR-I) = 0
                   MOVE 1 TO AN-L-QTY(AR-I)
               END-IF
               MOVE AN-L-QTY(AR-I) TO AR-QTY-ED
               MOVE FUNCTION TRIM(AR-QTY-ED) TO SQLIO-PARM-VAL(5)
               COMPUTE AR-AMT1 ROUNDED =
                   AN-L-NET(AR-I) / AN-L-QTY(AR-I)
               MOVE AR-AMT1 TO AR-QTY-ED
               MOVE FUNCTION TRIM(AR-QTY-ED) TO SQLIO-PARM-VAL(6)
               MOVE AN-L-NET(AR-I) TO AR-MONEY-ED
               MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(7)
               MOVE AN-L-VAT-CODE(AR-I) TO SQLIO-PARM-VAL(8)
               MOVE AN-L-VAT-RATE(AR-I) TO AR-MONEY-ED
               MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(9)
               MOVE AN-L-VAT-AMT(AR-I) TO AR-MONEY-ED
               MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(10)
               IF AN-CURRENCY = "GBP" OR AN-FX-RATE = 0
                   MOVE AN-L-NET(AR-I) TO AR-AMT1
                   MOVE AN-L-VAT-AMT(AR-I) TO AR-AMT2
               ELSE
                   COMPUTE AR-AMT1 ROUNDED =
                       AN-L-NET(AR-I) / AN-FX-RATE
                   COMPUTE AR-AMT2 ROUNDED =
                       AN-L-VAT-AMT(AR-I) / AN-FX-RATE
               END-IF
               MOVE AR-AMT1 TO AR-MONEY-ED
               MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(11)
               MOVE AR-AMT2 TO AR-MONEY-ED
               MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(12)
               MOVE AN-L-REV-ACCOUNT(AR-I) TO SQLIO-PARM-VAL(13)
               MOVE AN-L-ORIG-LINE(AR-I) TO AR-INT-ED
               MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(14)
               PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 14
                   MOVE "N" TO SQLIO-PARM-NULL(AR-J)
               END-PERFORM
               MOVE 0 TO SQLIO-COL-CNT
               MOVE 1 TO SQLIO-CURSOR
               MOVE "EXEC" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
               IF SQLIO-ERROR
                   PERFORM 9300-SQL-FAILED
               ELSE
                   MOVE "CLOS" TO SQLIO-FUNC
                   CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
               END-IF
           END-PERFORM.
       1400-EXIT.
           EXIT.

      *>   DR sales returns, DR output VAT, CR trade debtors
       1600-POST-JOURNAL SECTION.
       1600-START.
           INITIALIZE WS-POSTGL-CA
           MOVE "POST" TO CA-FUNCTION OF WS-POSTGL-CA
           MOVE AR-CO TO CA-COMPANY OF WS-POSTGL-CA
           MOVE AR-USERID TO CA-USER OF WS-POSTGL-CA
           MOVE AR-CORREL TO CA-CORRELATION-ID OF WS-POSTGL-CA
           MOVE AN-POSTING-DATE TO CA-BUSINESS-DATE OF WS-POSTGL-CA
           MOVE "AR.CREDITNOTE" TO PG-EVENT-CODE
           MOVE "AR" TO PG-JOURNAL-TYPE
           MOVE "AR" TO PG-SOURCE-MODULE
           MOVE AN-CREDIT-NO TO PG-SOURCE-REF
           MOVE AN-DOC-DATE TO PG-DOC-DATE
           MOVE AN-POSTING-DATE TO PG-POSTING-DATE
           MOVE AN-CURRENCY TO PG-CURRENCY
           MOVE AN-FX-RATE TO PG-FX-RATE
           MOVE "N" TO PG-OWN-TRAN
           MOVE SPACES TO PG-DESCRIPTION
           STRING "Credit note " FUNCTION TRIM(AN-CREDIT-NO)
                  " against " FUNCTION TRIM(AN-ORIG-INVOICE)
               DELIMITED BY SIZE INTO PG-DESCRIPTION
           MOVE 0 TO WS-LEG
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > AN-LINE-CNT
               ADD 1 TO WS-LEG
               MOVE "RETURNS" TO PG-G-LEG-CODE(WS-LEG)
               MOVE AN-L-NET(AR-I) TO PG-G-DEBIT(WS-LEG)
               MOVE AN-L-DESC(AR-I) TO PG-G-NARRATIVE(WS-LEG)
               MOVE AN-L-VAT-CODE(AR-I) TO PG-G-VAT-CODE(WS-LEG)
           END-PERFORM
           IF AN-VAT NOT = 0
               ADD 1 TO WS-LEG
               MOVE "VAT" TO PG-G-LEG-CODE(WS-LEG)
               MOVE AN-VAT TO PG-G-DEBIT(WS-LEG)
               MOVE SPACES TO PG-G-NARRATIVE(WS-LEG)
               STRING "Output VAT credited on "
                      FUNCTION TRIM(AN-CREDIT-NO)
                   DELIMITED BY SIZE INTO PG-G-NARRATIVE(WS-LEG)
           END-IF
           ADD 1 TO WS-LEG
           MOVE "DEBTOR" TO PG-G-LEG-CODE(WS-LEG)
           MOVE WS-O-CONTROL TO PG-G-ACCOUNT(WS-LEG)
           MOVE AN-GROSS TO PG-G-CREDIT(WS-LEG)
           MOVE "AR" TO PG-G-SUBLEDGER(WS-LEG)
           MOVE AN-CUSTOMER TO PG-G-SUBREF(WS-LEG)
           MOVE AN-REASON TO PG-G-NARRATIVE(WS-LEG)
           MOVE WS-LEG TO PG-LEG-CNT
           CALL "ARPOSTGL" USING WS-POSTGL-CA
           IF CA-RETURN-CODE OF WS-POSTGL-CA > 4
               MOVE CA-RETURN-CODE OF WS-POSTGL-CA TO AR-RC
               MOVE CA-REASON-CODE OF WS-POSTGL-CA TO AR-REASON
               MOVE CA-MESSAGE OF WS-POSTGL-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 1600-EXIT
           END-IF
           MOVE PG-JOURNAL-NO TO AN-JOURNAL-NO.
       1600-EXIT.
           EXIT.

       1700-WRITE-VAT-RECORDS SECTION.
       1700-START.
           PERFORM VARYING AR-I FROM 1 BY 1
               UNTIL AR-I > WS-VAT-CNT OR AR-RC-BAD
               INITIALIZE WS-VATW-CA
               MOVE "WRITE" TO CA-FUNCTION OF WS-VATW-CA
               MOVE AR-CO TO CA-COMPANY OF WS-VATW-CA
               MOVE AR-USERID TO CA-USER OF WS-VATW-CA
               MOVE AN-TAX-POINT TO CA-BUSINESS-DATE OF WS-VATW-CA
               MOVE "AR" TO VW-SOURCE-MODULE
               MOVE "SALESCREDIT" TO VW-DOC-TYPE
               MOVE AN-CREDIT-NO TO VW-DOC-REF
               MOVE "CUSTOMER" TO VW-PARTY-TYPE
               MOVE AN-CUSTOMER TO VW-PARTY-CODE
               MOVE WS-O-VATNO TO VW-PARTY-VAT-NO
               MOVE AN-TAX-POINT TO VW-TAX-POINT
               MOVE AN-POSTING-DATE TO VW-POSTING-DATE
               MOVE WS-V-CODE(AR-I) TO VW-VAT-CODE
               MOVE "OUTPUT" TO VW-DIRECTION
               MOVE AN-CURRENCY TO VW-CURRENCY
               MOVE AN-FX-RATE TO VW-FX-RATE
               MOVE PG-JOURNAL-ID TO VW-JOURNAL-ID
               COMPUTE VW-NET = 0 - WS-V-NET(AR-I)
               COMPUTE VW-VAT = 0 - WS-V-VAT(AR-I)
               COMPUTE VW-NET-GBP = 0 - WS-V-NET(AR-I)
               COMPUTE VW-VAT-GBP = 0 - WS-V-VAT(AR-I)
               MOVE AN-ORIG-INVOICE TO VW-EVIDENCE-REF
               CALL "ARVATWRT" USING WS-VATW-CA
               IF CA-RETURN-CODE OF WS-VATW-CA > 4
                   MOVE CA-RETURN-CODE OF WS-VATW-CA TO AR-RC
                   MOVE CA-REASON-CODE OF WS-VATW-CA TO AR-REASON
                   MOVE CA-MESSAGE OF WS-VATW-CA TO AR-MSG
                   ADD 1 TO AR-MSG-CNT
               END-IF
           END-PERFORM.
       1700-EXIT.
           EXIT.

      *>   a credit is an open item with a negative residual, so it is
      *>   allocatable against any eligible invoice of the same debtor
       1800-WRITE-OPEN-ITEM SECTION.
       1800-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT ar.OpenItem (CompanyCode, "
                  "CustomerCode, ItemType, DocumentNumber, "
                  "DocumentDate, DueDate, PostingDate, FiscalYear, "
                  "PeriodNo, CurrencyCode, FxRate, OriginalAmount, "
                  "OriginalAmountGBP, ResidualAmount, "
                  "ResidualAmountGBP, VatAmountGBP, SourceInvoiceId, "
                  "CreatedBy) VALUES (?, ?, 'CREDIT', ?, ?, ?, ?, "
                  "NULLIF(?,0), NULLIF(?,0), ?, ?, ?, ?, ?, ?, ?, ?, "
                  "?); SELECT CONVERT(varchar(24), SCOPE_IDENTITY()),"
                  " ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 18 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE AN-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE AN-CREDIT-NO TO SQLIO-PARM-VAL(3)
           MOVE AN-DOC-DATE TO SQLIO-PARM-VAL(4)
           MOVE AN-DOC-DATE TO SQLIO-PARM-VAL(5)
           MOVE AN-POSTING-DATE TO SQLIO-PARM-VAL(6)
           MOVE PC-FISCAL-YEAR TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(7)
           MOVE PC-PERIOD-NO TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(8)
           MOVE AN-CURRENCY TO SQLIO-PARM-VAL(9)
           MOVE AN-FX-RATE TO AR-RATE-ED
           MOVE FUNCTION TRIM(AR-RATE-ED) TO SQLIO-PARM-VAL(10)
           COMPUTE AR-AMT1 = 0 - AN-GROSS
           MOVE AR-AMT1 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(11)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(13)
           COMPUTE AR-AMT2 = 0 - AN-GROSS-GBP
           MOVE AR-AMT2 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(12)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(14)
           COMPUTE AR-AMT3 = 0 - AN-VAT-GBP
           MOVE AR-AMT3 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(15)
           MOVE AN-CREDIT-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(16)
           MOVE AR-USERID TO SQLIO-PARM-VAL(17)
           MOVE AR-USERID TO SQLIO-PARM-VAL(18)
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
               GO TO 1800-EXIT
           END-IF
           COMPUTE AN-OPEN-ITEM-ID = FUNCTION NUMVAL(WS-NEW-ID).
       1800-EXIT.
           EXIT.

       1900-MARK-POSTED SECTION.
       1900-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.SalesInvoice SET Status = 'POSTED', "
                  "PostedBy = ?, PostedAt = SYSDATETIME(), "
                  "IssuedAt = SYSDATETIME(), JournalId = "
                  "(SELECT TOP (1) j.JournalId FROM fin.Journal j "
                  "WHERE j.CompanyCode = ? AND j.JournalNumber = ?) "
                  "WHERE InvoiceId = ?; "
                  "UPDATE ar.SalesInvoice SET CreditableRemainingGBP "
                  "= ? WHERE CompanyCode = ? AND InvoiceNumber = ? "
                  "AND DocumentType = 'INVOICE'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 7 TO SQLIO-PARM-CNT
           MOVE AR-USERID TO SQLIO-PARM-VAL(1)
           MOVE AR-CO TO SQLIO-PARM-VAL(2)
           MOVE AN-JOURNAL-NO TO SQLIO-PARM-VAL(3)
           MOVE AN-CREDIT-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(4)
           COMPUTE AR-AMT1 = AN-CREDITABLE-REM - AN-GROSS-GBP
           MOVE AR-AMT1 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(5)
           MOVE AR-CO TO SQLIO-PARM-VAL(6)
           MOVE AN-ORIG-INVOICE TO SQLIO-PARM-VAL(7)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 7
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1900-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           COMPUTE AN-CREDITABLE-REM =
               AN-CREDITABLE-REM - AN-GROSS-GBP.
       1900-EXIT.
           EXIT.

       1950-IDEMPOTENT-END SECTION.
       1950-START.
           INITIALIZE WS-IDEM-CA
           MOVE "END" TO CA-FUNCTION OF WS-IDEM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-IDEM-CA
           MOVE AR-USERID TO CA-USER OF WS-IDEM-CA
           MOVE AR-CORREL TO CA-CORRELATION-ID OF WS-IDEM-CA
           MOVE AN-POSTING-DATE TO CA-BUSINESS-DATE OF WS-IDEM-CA
           MOVE "ar.SalesCredit.Post" TO ID-OPERATION
           MOVE "COMPLETED" TO ID-STATUS
           MOVE AN-CREDIT-NO TO ID-RESULT-REF
           MOVE "Sales credit note posted" TO ID-RESULT-MESSAGE
           CALL "IDEMPOT" USING WS-IDEM-CA.
       1950-EXIT.
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
           MOVE "ARC0098" TO AR-REASON
           MOVE SPACES TO AR-MSG
           STRING "ARCRNOTE does not support function ["
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
