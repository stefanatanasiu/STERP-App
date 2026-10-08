      *>================================================================
      *> PROGRAM-ID : SOROT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : TERMS AND CONDITIONS OF SALE AND RETENTION OF
      *>              TITLE.  TERMS RECORDS THE VERSION OF THE TERMS
      *>              THAT APPLIES TO AN ORDER AND WHETHER RETENTION
      *>              OF TITLE APPLIES.  UNPAIDLS PRODUCES, FOR ONE
      *>              CUSTOMER, THE LIST OF GOODS SUPPLIED UNDER
      *>              RETENTION OF TITLE THAT REMAIN UNPAID, SHOWING
      *>              THE ITEM, THE LOT OR SERIAL NUMBER, THE INVOICE
      *>              NUMBER, THE INVOICE DATE AND THE DESPATCH DATE
      *>              WITH THE UNPAID AMOUNT (SO-029, AC-124).
      *>              IT JOINS so.Despatch AND so.DespatchLine TO THE
      *>              SALES LEDGER'S ar.SalesInvoice AND
      *>              ar.SalesInvoiceLine, AND EXCLUDES ANYTHING THE
      *>              OPEN ITEM ACCOUNT SHOWS AS SETTLED.  THE SALES
      *>              LEDGER IS READ, NEVER WRITTEN.
      *> CALLED BY  : STERP DESKTOP CLIENT (SALES ENQUIRIES), ARINSOLV
      *> INTERFACE  : CALL "SOROT" USING DFHCOMMAREA (COPY SOROTCA)
      *> CALLS      : AUDITLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1993-10-11 RJH  CR0197 ROMALPA CLAUSE - GOODS NOT PAID FOR
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT YEARS ON INVOICE DATES
      *> 2002-05-29 DJM  CR1009 SERIAL NUMBERS ON THE ROT LIST
      *> 2026-10-06 STERP SO-029 UNPAID GOODS UNDER RETENTION OF TITLE
      *>================================================================
      *> REQ: SO-029
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOROT.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 11/10/1993.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUDIT-CA==.
       01  WS-CONTROL.
           05  WS-FUNC                 PIC X(8).
           05  WS-CO                   PIC X(10).
           05  WS-SITE                 PIC X(10).
           05  WS-USER                 PIC X(20).
           05  WS-BDATE                PIC 9(8).
           05  WS-CORR                 PIC X(36).
           05  WS-ERROR                PIC X VALUE "N".
           05  WS-RC                   PIC 9(2) VALUE 0.
           05  WS-REASON               PIC X(8).
           05  WS-MSG                  PIC X(200).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-N                    PIC S9(15)V9(6) VALUE 0.
           05  WS-ROWS                 PIC S9(4) VALUE 0.
           05  WS-AMT1                 PIC S9(13)V99 VALUE 0.
           05  WS-AMT2                 PIC S9(13)V99 VALUE 0.
           05  WS-RATIO                PIC S9(5)V9(9) VALUE 0.
           05  WS-LAST-INVOICE         PIC X(30) VALUE SPACES.
           05  WS-AMT-ED               PIC -(11)9.99.
           05  WS-INT-ED               PIC -(8)9.
       01  WS-ORDER-ROW.
           05  WO-ORDER-ID             PIC X(24).
           05  WO-CUSTOMER             PIC X(20).
           05  WO-TERMS-CODE           PIC X(12).
           05  WO-TERMS-VERSION        PIC X(20).
           05  WO-TERMS-SNAPSHOT       PIC X(100).
           05  WO-ROT                  PIC X(2).
           05  WO-STATUS               PIC X(12).
       01  WS-ROT-ROW.
           05  WR-ITEM                 PIC X(40).
           05  WR-DESC                 PIC X(60).
           05  WR-LOT                  PIC X(30).
           05  WR-SERIAL               PIC X(40).
           05  WR-QTY                  PIC X(24).
           05  WR-UOM                  PIC X(10).
           05  WR-INVOICE-NO           PIC X(30).
           05  WR-INV-DATE             PIC X(8).
           05  WR-DESP-REF             PIC X(30).
           05  WR-DESP-DATE            PIC X(8).
           05  WR-ORDER-NO             PIC X(30).
           05  WR-LINE-NET             PIC X(24).
           05  WR-RESIDUAL             PIC X(24).
           05  WR-ORIGINAL             PIC X(24).
           05  WR-SETTLEMENT           PIC X(12).
       01  WS-ONE-COL.
           05  WS-C1                   PIC X(40).
       LINKAGE SECTION.
       COPY SOROTCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 1000-INITIALISE THRU 1000-EXIT
           IF WS-ERROR = "N"
               EVALUATE WS-FUNC
                   WHEN "TERMS"
                       PERFORM 2000-TERMS THRU 2000-EXIT
                   WHEN "UNPAIDLS"
                       PERFORM 3000-UNPAID-LIST THRU 3000-EXIT
                   WHEN OTHER
                       PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
               END-EVALUATE
           END-IF
           PERFORM 9400-REPORT THRU 9400-EXIT.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INITIALISE SECTION.
       1000-START.
           MOVE "N" TO WS-ERROR
           MOVE 0 TO WS-RC WS-I WS-J WS-ROWS
           MOVE SPACES TO WS-REASON WS-MSG WS-LAST-INVOICE
           MOVE "SOROT" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "SO-029" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE FUNCTION UPPER-CASE(CA-FUNCTION OF DFHCOMMAREA)
               TO WS-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO WS-CO
           MOVE CA-SITE OF DFHCOMMAREA TO WS-SITE
           MOVE CA-USER OF DFHCOMMAREA TO WS-USER
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO WS-BDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO WS-CORR
           MOVE "N" TO RO-ORDER-FOUND
           MOVE "N" TO RO-UPDATED
           MOVE "N" TO RO-TRUNCATED
           MOVE 0 TO RO-ITEM-CNT
           MOVE 0 TO RO-INVOICE-CNT
           MOVE 0 TO RO-TOTAL-UNPAID-GBP
           MOVE 0 TO RO-TOTAL-ROT-GBP
           MOVE 0 TO RO-TOTAL-ROT-QTY
           IF WS-BDATE = 0
               MOVE 20261006 TO WS-BDATE
               MOVE WS-BDATE TO CA-BUSINESS-DATE OF DFHCOMMAREA
           END-IF
           IF RO-AS-AT-DATE = 0
               MOVE WS-BDATE TO RO-AS-AT-DATE
           END-IF
           IF WS-CO = SPACES
               MOVE "ROT0001" TO WS-REASON
               MOVE "A company code is required" TO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 2000  TERMS.  THE ORDER CARRIES THE VERSION OF THE TERMS AND
      *>       CONDITIONS OF SALE THAT APPLIES TO IT.  WITH NO VERSION
      *>       SUPPLIED THE HELD VALUES ARE SIMPLY READ BACK, SO THE
      *>       SAME FUNCTION ANSWERS "WHAT APPLIES?" AND "THIS
      *>       APPLIES".
      *>----------------------------------------------------------------
       2000-TERMS SECTION.
       2000-START.
           IF RO-ORDER-NUMBER = SPACES
               MOVE "ROT0002" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "An order number is required to record or "
                      "read the terms of sale"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           PERFORM 8000-LOAD-ORDER THRU 8000-EXIT
           IF WS-ERROR = "Y"
               GO TO 2000-EXIT
           END-IF
           IF RO-TERMS-VERSION = SPACES AND RO-ROT-APPLIES = SPACES
      *>       a read only enquiry
               MOVE SPACES TO WS-MSG
               STRING "Order " FUNCTION TRIM(RO-ORDER-NUMBER)
                      " is on terms version "
                      FUNCTION TRIM(RO-TERMS-VERSION)
                   DELIMITED BY SIZE INTO WS-MSG
               MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
               MOVE SPACES TO WS-MSG
               GO TO 2000-EXIT
           END-IF
           IF RO-TERMS-VERSION = SPACES
               MOVE "ROT0003" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "State the version of the terms and "
                      "conditions of sale that applies"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF RO-ROT-APPLIES = SPACES
               MOVE "Y" TO RO-ROT-APPLIES
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrder SET "
                  "TermsAndConditionsVersion = ?, RetentionOfTitle "
                  "= ?, TermsCode = COALESCE(NULLIF(?,''), "
                  "TermsCode), TermsSnapshot = "
                  "COALESCE(NULLIF(?,''), TermsSnapshot), "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE CompanyCode = ? AND OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE RO-TERMS-VERSION TO SQLIO-PARM-VAL(1)
           IF RO-ROT-YES
               MOVE "1" TO SQLIO-PARM-VAL(2)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(2)
           END-IF
           MOVE RO-TERMS-CODE TO SQLIO-PARM-VAL(3)
           MOVE RO-TERMS-SNAPSHOT TO SQLIO-PARM-VAL(4)
           MOVE WS-USER TO SQLIO-PARM-VAL(5)
           MOVE WS-CO TO SQLIO-PARM-VAL(6)
           MOVE RO-ORDER-NUMBER TO SQLIO-PARM-VAL(7)
           MOVE 7 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-ERROR = "Y"
               GO TO 2000-EXIT
           END-IF
           MOVE "Y" TO RO-UPDATED
           PERFORM 8700-AUDIT THRU 8700-EXIT
           MOVE SPACES TO WS-MSG
           IF RO-ROT-YES
               STRING "Order " FUNCTION TRIM(RO-ORDER-NUMBER)
                      " is on terms version "
                      FUNCTION TRIM(RO-TERMS-VERSION)
                      " with retention of title"
                   DELIMITED BY SIZE INTO WS-MSG
           ELSE
               STRING "Order " FUNCTION TRIM(RO-ORDER-NUMBER)
                      " is on terms version "
                      FUNCTION TRIM(RO-TERMS-VERSION)
                      " without retention of title"
                   DELIMITED BY SIZE INTO WS-MSG
           END-IF
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE SPACES TO WS-MSG.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 3000  UNPAIDLS.  THE JOIN RUNS DESPATCH LINE -> DESPATCH ->
      *>       SALES ORDER (FOR THE RETENTION OF TITLE FLAG) ->
      *>       SALES INVOICE -> SALES INVOICE LINE -> OPEN ITEM.  THE
      *>       OPEN ITEM DECIDES WHAT IS STILL UNPAID, SO A SETTLED
      *>       INVOICE NEVER APPEARS, AND A PART PAID INVOICE SHOWS
      *>       ITS LINES IN PROPORTION TO WHAT IS STILL OUTSTANDING.
      *>----------------------------------------------------------------
       3000-UNPAID-LIST SECTION.
       3000-START.
           IF RO-CUSTOMER = SPACES
               MOVE "ROT0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A customer code is required for the "
                      "retention of title list"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT dl.ItemCode, ISNULL(dl.Description,''), "
                  "ISNULL(dl.LotCode,''), ISNULL(dl.SerialNo,''), "
                  "CAST(dl.DespatchedQty AS varchar(24)), "
                  "ISNULL(dl.UomCode,''), si.InvoiceNumber, "
                  "CONVERT(char(8), si.DocumentDate, 112), "
                  "d.DespatchRef, CONVERT(char(8), d.DespatchDate, "
                  "112), ISNULL(d.OrderNumber,''), "
                  "CAST(ISNULL(sil.NetAmountGBP,0) AS varchar(24)), "
                  "CAST(oi.ResidualAmountGBP AS varchar(24)), "
                  "CAST(oi.OriginalAmountGBP AS varchar(24)), "
                  "oi.SettlementStatus "
               DELIMITED BY SIZE INTO SQLIO-STMT
           STRING FUNCTION TRIM(SQLIO-STMT)
                  " FROM so.Despatch d JOIN so.DespatchLine dl ON "
                  "dl.DespatchId = d.DespatchId JOIN so.SalesOrder "
                  "o ON o.CompanyCode = d.CompanyCode AND "
                  "o.OrderNumber = d.OrderNumber JOIN "
                  "ar.SalesInvoice si ON si.CompanyCode = "
                  "d.CompanyCode AND si.DespatchRef = d.DespatchRef "
                  "AND si.DocumentType = 'INVOICE' LEFT JOIN "
                  "ar.SalesInvoiceLine sil ON sil.InvoiceId = "
                  "si.InvoiceId AND sil.DespatchLine = dl.LineNum "
                  "JOIN ar.OpenItem oi ON oi.CompanyCode = "
                  "si.CompanyCode AND oi.ItemType = 'INVOICE' AND "
                  "oi.DocumentNumber = si.InvoiceNumber "
               DELIMITED BY SIZE INTO SQLIO-STMT
           STRING FUNCTION TRIM(SQLIO-STMT)
                  " WHERE d.CompanyCode = ? AND d.CustomerCode = ? "
                  "AND d.Status = 'CONFIRMED' AND "
                  "o.RetentionOfTitle = 1 AND oi.SettlementStatus "
                  "NOT IN ('SETTLED','REVERSED') AND "
                  "oi.ResidualAmountGBP > 0 AND d.DespatchDate <= "
                  "CONVERT(date, ?, 112) ORDER BY si.InvoiceNumber, "
                  "d.DespatchRef, dl.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE RO-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE RO-AS-AT-DATE TO SQLIO-PARM-VAL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           MOVE 15 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 60 TO SQLIO-COL-WIDTH(2)
           MOVE 30 TO SQLIO-COL-WIDTH(3)
           MOVE 40 TO SQLIO-COL-WIDTH(4)
           MOVE 24 TO SQLIO-COL-WIDTH(5)
           MOVE 10 TO SQLIO-COL-WIDTH(6)
           MOVE 30 TO SQLIO-COL-WIDTH(7)
           MOVE 8 TO SQLIO-COL-WIDTH(8)
           MOVE 30 TO SQLIO-COL-WIDTH(9)
           MOVE 8 TO SQLIO-COL-WIDTH(10)
           MOVE 30 TO SQLIO-COL-WIDTH(11)
           MOVE 24 TO SQLIO-COL-WIDTH(12)
           MOVE 24 TO SQLIO-COL-WIDTH(13)
           MOVE 24 TO SQLIO-COL-WIDTH(14)
           MOVE 12 TO SQLIO-COL-WIDTH(15)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROT-ROW
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
               GO TO 3000-EXIT
           END-IF
           MOVE 0 TO WS-ROWS
           PERFORM 3100-ONE-ITEM THRU 3100-EXIT
               UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR OR WS-ROWS >= 80
           IF NOT SQLIO-NOTFOUND AND NOT SQLIO-ERROR
               MOVE "Y" TO RO-TRUNCATED
           END-IF
           MOVE 4 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROT-ROW
           MOVE WS-ROWS TO RO-ITEM-CNT
           IF WS-ROWS = 0
               MOVE 4 TO WS-RC
               MOVE "ROT0004" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "No goods supplied to "
                      FUNCTION TRIM(RO-CUSTOMER)
                      " under retention of title remain unpaid"
                   DELIMITED BY SIZE INTO WS-MSG
               GO TO 3000-EXIT
           END-IF
           IF RO-TRUNCATED = "Y"
               MOVE 4 TO WS-RC
               MOVE "ROT0005" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "The retention of title list was cut at 80 "
                      "items; narrow the enquiry by date"
                   DELIMITED BY SIZE INTO WS-MSG
               GO TO 3000-EXIT
           END-IF
           MOVE RO-TOTAL-ROT-GBP TO WS-AMT-ED
           MOVE WS-ROWS TO WS-INT-ED
           MOVE SPACES TO WS-MSG
           STRING FUNCTION TRIM(WS-INT-ED)
                  " item line(s) supplied under retention of title "
                  "remain unpaid, £" FUNCTION TRIM(WS-AMT-ED)
                  " in total"
               DELIMITED BY SIZE INTO WS-MSG
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE SPACES TO WS-MSG.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3100-ONE-ITEM SECTION.
       3100-START.
           MOVE 4 TO SQLIO-CURSOR
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROT-ROW
           IF SQLIO-NOTFOUND OR SQLIO-ERROR
               GO TO 3100-EXIT
           END-IF
           ADD 1 TO WS-ROWS
           MOVE WS-ROWS TO WS-I
           MOVE WR-ITEM TO RO-I-ITEM(WS-I)
           MOVE WR-DESC TO RO-I-DESC(WS-I)
           MOVE WR-LOT TO RO-I-LOT(WS-I)
           MOVE WR-SERIAL TO RO-I-SERIAL(WS-I)
           COMPUTE WS-N = FUNCTION NUMVAL(WR-QTY)
           MOVE WS-N TO RO-I-QTY(WS-I)
           ADD WS-N TO RO-TOTAL-ROT-QTY
           MOVE WR-UOM TO RO-I-UOM(WS-I)
           MOVE WR-INVOICE-NO TO RO-I-INVOICE-NO(WS-I)
           COMPUTE WS-N = FUNCTION NUMVAL(WR-INV-DATE)
           MOVE WS-N TO RO-I-INV-DATE(WS-I)
           MOVE WR-DESP-REF TO RO-I-DESP-REF(WS-I)
           COMPUTE WS-N = FUNCTION NUMVAL(WR-DESP-DATE)
           MOVE WS-N TO RO-I-DESP-DATE(WS-I)
           MOVE WR-ORDER-NO TO RO-I-ORDER-NO(WS-I)
           MOVE WR-SETTLEMENT TO RO-I-SETTLEMENT(WS-I)
           COMPUTE WS-N = FUNCTION NUMVAL(WR-LINE-NET)
           MOVE WS-N TO WS-AMT1
           MOVE WS-AMT1 TO RO-I-NET-GBP(WS-I)
           PERFORM 3200-UNPAID-SHARE THRU 3200-EXIT
           MOVE WS-AMT2 TO RO-I-UNPAID-GBP(WS-I)
           ADD WS-AMT2 TO RO-TOTAL-ROT-GBP
      *>   the invoice residual is counted once per invoice, however
      *>   many of its lines carry retained title
           IF WR-INVOICE-NO NOT = WS-LAST-INVOICE
               MOVE WR-INVOICE-NO TO WS-LAST-INVOICE
               ADD 1 TO RO-INVOICE-CNT
               COMPUTE WS-N = FUNCTION NUMVAL(WR-RESIDUAL)
               ADD WS-N TO RO-TOTAL-UNPAID-GBP
           END-IF.
       3100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 3200  A PART PAID INVOICE DOES NOT RELEASE PARTICULAR GOODS,
      *>       SO THE UNPAID AMOUNT AGAINST EACH LINE IS ITS SHARE OF
      *>       WHAT IS STILL OUTSTANDING.  WITH NOTHING PAID THE SHARE
      *>       IS THE WHOLE LINE.
      *>----------------------------------------------------------------
       3200-UNPAID-SHARE SECTION.
       3200-START.
           COMPUTE WS-N = FUNCTION NUMVAL(WR-ORIGINAL)
           IF WS-N = 0
               MOVE WS-AMT1 TO WS-AMT2
               GO TO 3200-EXIT
           END-IF
           MOVE WS-N TO WS-RATIO
           COMPUTE WS-N = FUNCTION NUMVAL(WR-RESIDUAL)
           COMPUTE WS-RATIO ROUNDED = WS-N / WS-RATIO
           IF WS-RATIO > 1
               MOVE 1 TO WS-RATIO
           END-IF
           IF WS-RATIO < 0
               MOVE 0 TO WS-RATIO
           END-IF
           COMPUTE WS-AMT2 ROUNDED = WS-AMT1 * WS-RATIO.
       3200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8000-LOAD-ORDER SECTION.
       8000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(SalesOrderId AS varchar(24)), "
                  "CustomerCode, ISNULL(TermsCode,''), "
                  "ISNULL(TermsAndConditionsVersion,''), "
                  "ISNULL(TermsSnapshot,''), "
                  "CAST(RetentionOfTitle AS char(1)), "
                  "CommercialStatus FROM so.SalesOrder WHERE "
                  "CompanyCode = ? AND OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE RO-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           MOVE 7 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 12 TO SQLIO-COL-WIDTH(3)
           MOVE 20 TO SQLIO-COL-WIDTH(4)
           MOVE 100 TO SQLIO-COL-WIDTH(5)
           MOVE 2 TO SQLIO-COL-WIDTH(6)
           MOVE 12 TO SQLIO-COL-WIDTH(7)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ORDER-ROW
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
               GO TO 8000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO WS-RC
               MOVE "Y" TO WS-ERROR
               MOVE "ROT0002" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Order " FUNCTION TRIM(RO-ORDER-NUMBER)
                      " is not held for this company"
                   DELIMITED BY SIZE INTO WS-MSG
               GO TO 8000-EXIT
           END-IF
           MOVE "Y" TO RO-ORDER-FOUND
           MOVE WO-CUSTOMER TO RO-CUSTOMER
           IF RO-TERMS-CODE = SPACES
               MOVE WO-TERMS-CODE TO RO-TERMS-CODE
           END-IF
           IF RO-TERMS-VERSION = SPACES AND WS-FUNC NOT = "TERMS"
               MOVE WO-TERMS-VERSION TO RO-TERMS-VERSION
           END-IF
           IF RO-TERMS-SNAPSHOT = SPACES
               MOVE WO-TERMS-SNAPSHOT TO RO-TERMS-SNAPSHOT
           END-IF
           IF RO-ROT-APPLIES = SPACES
               IF WO-ROT(1:1) = "1"
                   MOVE "Y" TO RO-ROT-APPLIES
               ELSE
                   MOVE "N" TO RO-ROT-APPLIES
               END-IF
               MOVE WO-TERMS-VERSION TO RO-TERMS-VERSION
           END-IF.
       8000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8500-EXEC-UPDATE SECTION.
       8500-START.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
           END-IF
           MOVE 1 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       8500-EXIT.
           EXIT.

       8650-NO-NULLS SECTION.
       8650-START.
           PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM.
       8650-EXIT.
           EXIT.

       8700-AUDIT SECTION.
       8700-START.
           INITIALIZE WS-AUDIT-CA
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUDIT-CA
           MOVE WS-CO TO CA-COMPANY OF WS-AUDIT-CA
           MOVE WS-SITE TO CA-SITE OF WS-AUDIT-CA
           MOVE WS-USER TO CA-USER OF WS-AUDIT-CA
           MOVE WS-BDATE TO CA-BUSINESS-DATE OF WS-AUDIT-CA
           MOVE WS-CORR TO CA-CORRELATION-ID OF WS-AUDIT-CA
           MOVE "so.SalesOrder" TO AU-ENTITY-TYPE
           MOVE RO-ORDER-NUMBER TO AU-ENTITY-KEY
           MOVE "TERMS" TO AU-ACTION
           MOVE "TermsAndConditionsVersion" TO AU-FIELD-NAME
           MOVE RO-TERMS-VERSION TO AU-NEW-VALUE
           MOVE SPACES TO AU-REASON
           IF RO-ROT-YES
               STRING "Retention of title applies"
                   DELIMITED BY SIZE INTO AU-REASON
           ELSE
               STRING "Retention of title does not apply"
                   DELIMITED BY SIZE INTO AU-REASON
           END-IF
           MOVE "SOROT" TO AU-SOURCE
           MOVE "N" TO AU-SENSITIVE
           CALL "AUDITLOG" USING WS-AUDIT-CA
           MOVE AU-AUDIT-ID TO RO-AUDIT-ID.
       8700-EXIT.
           EXIT.

       8900-SQL-FAILED SECTION.
       8900-START.
           MOVE "Y" TO WS-ERROR
           MOVE 12 TO WS-RC
           MOVE "SQL0001" TO WS-REASON
           MOVE SQLIO-ERRMSG(1:200) TO WS-MSG
           IF WS-MSG = SPACES
               MOVE "The database refused the request" TO WS-MSG
           END-IF.
       8900-EXIT.
           EXIT.

       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           IF WS-RC < 8
               MOVE 8 TO WS-RC
           END-IF.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "ROT0006" TO WS-REASON
           MOVE SPACES TO WS-MSG
           STRING "Function " FUNCTION TRIM(WS-FUNC)
                  " is not supported by SOROT; use TERMS or "
                  "UNPAIDLS"
               DELIMITED BY SIZE INTO WS-MSG
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9400-REPORT SECTION.
       9400-START.
           MOVE WS-RC TO CA-RETURN-CODE OF DFHCOMMAREA
           IF WS-REASON NOT = SPACES
               MOVE WS-REASON TO CA-REASON-CODE OF DFHCOMMAREA
           END-IF
           IF WS-MSG NOT = SPACES
               MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           END-IF
           IF CA-MESSAGE OF DFHCOMMAREA NOT = SPACES
               MOVE 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       9400-EXIT.
           EXIT.
