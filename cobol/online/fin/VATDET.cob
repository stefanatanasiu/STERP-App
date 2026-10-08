      *>================================================================
      *> PROGRAM-ID : VATDET
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER (VALUE ADDED TAX)
      *> PURPOSE    : VAT DETERMINATION.  FINDS THE EFFECTIVE DATED
      *>              RATE FOR A TAX POINT, DECIDES THE LIABILITY, THE
      *>              SPECIAL MECHANISM AND THE NINE BOX MAPPING, AND
      *>              CALCULATES THE TAX.
      *>              VAT IS ROUNDED ONCE PER LINE, NOT ONCE PER
      *>              INVOICE: TWO STANDARD RATED LINES OF 0.03 EACH
      *>              GIVE 0.01 + 0.01 = 0.02, NOT THE 0.01 AN INVOICE
      *>              LEVEL CALCULATION WOULD PRODUCE (AC-015).
      *>              A RATE CHANGE APPLIES BY TAX POINT ONLY AND NEVER
      *>              RECALCULATES POSTED VAT HISTORY (FIN-016).
      *>              REVERSE CHARGE SERVICES, THE DOMESTIC REVERSE
      *>              CHARGE AND POSTPONED IMPORT VAT SELF ACCOUNT:
      *>              THE SAME VALUE IS OUTPUT AND INPUT TAX AND NO
      *>              CASH VAT IS PAID TO THE SUPPLIER (FIN-028).
      *> CALLED BY  : AR, AP, VATRET9, .NET CLIENT
      *> INTERFACE  : CALL "VATDET" USING WS-VATDET-CA
      *>              COPYBOOK VATDETCA
      *> CALLS      : RNDALLOC, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-08-30 APW CR0120 ORIGINAL - MICRO FOCUS COBOL/2
      *> 1995-11-29 RJH CR0366 VAT FRACTION TABLE REMOVED, RATES NOW
      *>                HELD EFFECTIVE DATED ON THE DATABASE
      *> 2008-12-01 KMP CR2240 15 PER CENT TEMPORARY RATE
      *> 2011-01-04 KMP CR2501 20 PER CENT STANDARD RATE
      *> 2021-01-01 DJM CR3110 BREXIT - PVA, DRC, C79, EXPORT EVIDENCE
      *> 2026-10-05 STERP PER LINE ROUNDING AND CASH ACCOUNTING
      *>================================================================
      *> REQ: FIN-016, FIN-017, FIN-028, FIN-029
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VATDET.
       AUTHOR. A P WHITTAKER.
       DATE-WRITTEN. 30/08/1994.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY RNDALLOCCA REPLACING ==DFHCOMMAREA== BY ==WS-RNDALLOC-CA==
                      LEADING ==CA-== BY ==RH-==.
       01  WS-SWITCHES.
           05  WS-REJECTED             PIC X VALUE "N".
               88  WS-IS-REJECTED                VALUE "Y".
       01  WS-WORK.
           05  WS-IDX                  PIC S9(4) COMP-5 VALUE 0.
           05  WS-NET                  PIC S9(13)V99 VALUE 0.
           05  WS-VAT                  PIC S9(13)V99 VALUE 0.
           05  WS-BASIS                PIC S9(13)V9(6) VALUE 0.
           05  WS-DATE-TEXT            PIC X(8).
           05  WS-MONEY-ED             PIC -(11)9.99.
       01  WS-CODE-ROW.
           05  WS-C-NAME               PIC X(60).
           05  WS-C-LIABILITY          PIC X(12).
           05  WS-C-DIRECTION          PIC X(6).
           05  WS-C-MECHANISM          PIC X(12).
           05  WS-C-RECOVER            PIC X(4).
           05  WS-C-SELFACC            PIC X(4).
           05  WS-C-OUTVATBOX          PIC X(4).
           05  WS-C-INVATBOX           PIC X(4).
           05  WS-C-OUTNETBOX          PIC X(4).
           05  WS-C-INNETBOX           PIC X(4).
           05  WS-C-WORDING            PIC X(200).
           05  WS-C-EVIDENCE           PIC X(100).
           05  WS-C-ACTIVE             PIC X(4).
       01  WS-RATE-ROW.
           05  WS-R-RATE               PIC X(14).
           05  WS-R-FROM               PIC X(8).
       01  WS-PER-ROW.
           05  WS-E-KEY                PIC X(10).
           05  WS-E-STATUS             PIC X(14).
           05  WS-E-SCHEME             PIC X(12).
       LINKAGE SECTION.
       COPY VATDETCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           EVALUATE CA-FUNCTION
               WHEN "RATE"     PERFORM 1000-RATE THRU 1000-EXIT
               WHEN "LINE"     PERFORM 2000-LINE THRU 2000-EXIT
               WHEN "DOC"      PERFORM 3000-DOCUMENT THRU 3000-EXIT
               WHEN "SELFACC"  PERFORM 4000-SELF-ACCOUNT THRU 4000-EXIT
               WHEN "PERIOD"   PERFORM 5000-VAT-PERIOD THRU 5000-EXIT
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.

       0100-INITIALISE.
           MOVE "N" TO WS-REJECTED
           MOVE "VATDET" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "FIN-017" TO CA-REQ-REF
           MOVE 0 TO VD-RATE-PCT
           MOVE 0 TO VD-RATE-FROM
           MOVE 0 TO VD-VAT-AMOUNT
           MOVE 0 TO VD-VAT-AMOUNT-GBP
           MOVE 0 TO VD-NET-AMOUNT-GBP
           MOVE 0 TO VD-GROSS-AMOUNT
           MOVE 0 TO VD-TOTAL-NET
           MOVE 0 TO VD-TOTAL-VAT
           MOVE 0 TO VD-TOTAL-GROSS
           MOVE 0 TO VD-RECOVERABLE-VAT
           MOVE 0 TO VD-SELF-OUTPUT-VAT
           MOVE 0 TO VD-SELF-INPUT-VAT
           MOVE 0 TO VD-OUT-VAT-BOX
           MOVE 0 TO VD-IN-VAT-BOX
           MOVE 0 TO VD-OUT-NET-BOX
           MOVE 0 TO VD-IN-NET-BOX
           MOVE 0 TO VD-VAT-BOX
           MOVE 0 TO VD-NET-BOX
           MOVE SPACES TO VD-LIABILITY
           MOVE SPACES TO VD-MECHANISM
           MOVE SPACES TO VD-INVOICE-WORDING
           MOVE SPACES TO VD-EVIDENCE-REQD
           MOVE "N" TO VD-RECOVERABLE
           MOVE "N" TO VD-SELF-ACCOUNTS
           MOVE "N" TO VD-IS-LATE-ENTRY
           MOVE "BASIC" TO VD-TAX-POINT-KIND
           MOVE FUNCTION UPPER-CASE(VD-VAT-CODE) TO VD-VAT-CODE
           IF VD-DIRECTION = SPACES OR VD-DIRECTION = LOW-VALUES
               MOVE "OUTPUT" TO VD-DIRECTION
           END-IF
           MOVE FUNCTION UPPER-CASE(VD-DIRECTION) TO VD-DIRECTION
           IF VD-TAX-POINT = 0
               MOVE CA-BUSINESS-DATE TO VD-TAX-POINT
           END-IF
           IF VD-CURRENCY = SPACES
               MOVE "GBP" TO VD-CURRENCY
           END-IF
           IF VD-FX-RATE = 0
               MOVE 1 TO VD-FX-RATE
           END-IF
           MOVE VD-TAX-POINT TO WS-DATE-TEXT.
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000 - THE CODE AND ITS RATE AT THE TAX POINT
      *>----------------------------------------------------------------
       1000-RATE.
           IF VD-VAT-CODE = SPACES OR VD-VAT-CODE = LOW-VALUES
               MOVE "VAT0001" TO CA-REASON-CODE
               MOVE "A VAT code is required on every taxable line"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1000-EXIT
           END-IF
           PERFORM 1100-READ-CODE THRU 1100-EXIT
           IF CA-FAILED
               GO TO 1000-EXIT
           END-IF
           PERFORM 1200-READ-RATE THRU 1200-EXIT.
       1000-EXIT.
           EXIT.

       1100-READ-CODE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT v.Name, v.Liability, v.Direction, "
                  "v.Mechanism, CAST(v.IsRecoverable AS char(1)), "
                  "CAST(v.SelfAccounts AS char(1)), "
                  "CAST(ISNULL(v.OutputVatBox,0) AS varchar(4)), "
                  "CAST(ISNULL(v.InputVatBox,0) AS varchar(4)), "
                  "CAST(ISNULL(v.OutputNetBox,0) AS varchar(4)), "
                  "CAST(ISNULL(v.InputNetBox,0) AS varchar(4)), "
                  "ISNULL(v.InvoiceWording,''), "
                  "ISNULL(v.EvidenceRequired,''), "
                  "CAST(v.IsActive AS char(1)) FROM fin.VatCode v "
                  "WHERE v.VatCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE VD-VAT-CODE TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 13 TO SQLIO-COL-CNT
           MOVE 60 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 6 TO SQLIO-COL-WIDTH(3)
           MOVE 12 TO SQLIO-COL-WIDTH(4)
           MOVE 4 TO SQLIO-COL-WIDTH(5)
           MOVE 4 TO SQLIO-COL-WIDTH(6)
           MOVE 4 TO SQLIO-COL-WIDTH(7)
           MOVE 4 TO SQLIO-COL-WIDTH(8)
           MOVE 4 TO SQLIO-COL-WIDTH(9)
           MOVE 4 TO SQLIO-COL-WIDTH(10)
           MOVE 200 TO SQLIO-COL-WIDTH(11)
           MOVE 100 TO SQLIO-COL-WIDTH(12)
           MOVE 4 TO SQLIO-COL-WIDTH(13)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CODE-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 1100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "VAT0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "VAT code " FUNCTION TRIM(VD-VAT-CODE)
                      " is not held"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 1100-EXIT
           END-IF
           MOVE WS-C-LIABILITY TO VD-LIABILITY
           MOVE WS-C-MECHANISM TO VD-MECHANISM
           MOVE WS-C-WORDING TO VD-INVOICE-WORDING
           MOVE WS-C-EVIDENCE TO VD-EVIDENCE-REQD
           COMPUTE VD-OUT-VAT-BOX = FUNCTION NUMVAL(WS-C-OUTVATBOX)
           COMPUTE VD-IN-VAT-BOX = FUNCTION NUMVAL(WS-C-INVATBOX)
           COMPUTE VD-OUT-NET-BOX = FUNCTION NUMVAL(WS-C-OUTNETBOX)
           COMPUTE VD-IN-NET-BOX = FUNCTION NUMVAL(WS-C-INNETBOX)
           IF WS-C-RECOVER(1:1) = "1"
               MOVE "Y" TO VD-RECOVERABLE
           END-IF
           IF WS-C-SELFACC(1:1) = "1"
               MOVE "Y" TO VD-SELF-ACCOUNTS
           END-IF
           IF VD-DIRECTION = "OUTPUT"
               MOVE VD-OUT-VAT-BOX TO VD-VAT-BOX
               MOVE VD-OUT-NET-BOX TO VD-NET-BOX
           ELSE
               MOVE VD-IN-VAT-BOX TO VD-VAT-BOX
               MOVE VD-IN-NET-BOX TO VD-NET-BOX
           END-IF
           IF WS-C-ACTIVE(1:1) NOT = "1"
               MOVE 4 TO CA-RETURN-CODE
               MOVE "VAT0003" TO CA-REASON-CODE
               MOVE "That VAT code is no longer in use for new documen"
                   TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF
      *>   Direction is checked but a purchase only code used on a sale
      *>   is a refusal, not a silent swap.
           IF WS-C-DIRECTION = "SALE" AND VD-DIRECTION = "INPUT"
               MOVE "VAT0004" TO CA-REASON-CODE
               MOVE "That VAT code may only be used on sales"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1100-EXIT
           END-IF
           IF WS-C-DIRECTION = "PURCH" AND VD-DIRECTION = "OUTPUT"
             AND VD-MECHANISM NOT = "RCSERVICE"
             AND VD-MECHANISM NOT = "PVA"
               MOVE "VAT0005" TO CA-REASON-CODE
               MOVE "That VAT code may only be used on purchases"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1100-EXIT.
           EXIT.

      *> The rate in force at the tax point.  A later rate change does
      *> not reach back: the newest row not later than the tax point is
      *> the one that applies (FIN-016, AC-015).
       1200-READ-RATE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CAST(r.RatePct AS varchar(14)), "
                  "CONVERT(char(8), r.EffectiveFrom, 112) "
                  "FROM fin.VatRate r WHERE r.VatCode = ? "
                  "AND r.EffectiveFrom <= ? "
                  "ORDER BY r.EffectiveFrom DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE VD-VAT-CODE TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-DATE-TEXT TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 14 TO SQLIO-COL-WIDTH(1)
           MOVE 8 TO SQLIO-COL-WIDTH(2)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-RATE-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 1200-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "VAT0006" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No approved VAT rate is in force for "
                      FUNCTION TRIM(VD-VAT-CODE)
                      " at that tax point"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1200-EXIT
           END-IF
           COMPUTE VD-RATE-PCT = FUNCTION NUMVAL(WS-R-RATE)
           COMPUTE VD-RATE-FROM = FUNCTION NUMVAL(WS-R-FROM).
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000 - ONE LINE.  ROUNDED HALF AWAY FROM ZERO, PER LINE.
      *>----------------------------------------------------------------
       2000-LINE.
           PERFORM 1000-RATE THRU 1000-EXIT
           IF CA-FAILED
               GO TO 2000-EXIT
           END-IF
           MOVE VD-NET-AMOUNT TO WS-NET
           IF VD-DISCOUNT-PCT NOT = 0
      *>       VAT is due on what is actually received, so a prompt
      *>       payment discount reduces the taxable basis (FIN-017).
               COMPUTE WS-NET ROUNDED =
                   VD-NET-AMOUNT * (100 - VD-DISCOUNT-PCT) / 100
           END-IF
           COMPUTE WS-BASIS = WS-NET * VD-RATE-PCT / 100
           PERFORM 2100-ROUND-IT THRU 2100-EXIT
           MOVE WS-VAT TO VD-VAT-AMOUNT
      *>   The consideration actually received is the discounted net,
      *>   so the gross payable is that net plus the VAT charged on
      *>   it.  Adding the VAT to the undiscounted net overstated the
      *>   invoice total by the discount (FIN-017).
           COMPUTE VD-GROSS-AMOUNT = WS-NET + VD-VAT-AMOUNT
           IF VD-CURRENCY = "GBP" OR VD-FX-RATE = 1
               MOVE VD-VAT-AMOUNT TO VD-VAT-AMOUNT-GBP
               MOVE VD-NET-AMOUNT TO VD-NET-AMOUNT-GBP
           ELSE
      *>       The sterling VAT of a foreign currency invoice is fixed
      *>       at the invoice date and does not move with settlement.
               COMPUTE VD-VAT-AMOUNT-GBP ROUNDED =
                   VD-VAT-AMOUNT / VD-FX-RATE
               COMPUTE VD-NET-AMOUNT-GBP ROUNDED =
                   VD-NET-AMOUNT / VD-FX-RATE
           END-IF
           IF VD-RECOVERABLE = "Y" AND VD-DIRECTION = "INPUT"
               MOVE VD-VAT-AMOUNT-GBP TO VD-RECOVERABLE-VAT
           ELSE
               MOVE 0 TO VD-RECOVERABLE-VAT
           END-IF
           PERFORM 4000-SELF-ACCOUNT THRU 4000-EXIT
           PERFORM 5000-VAT-PERIOD THRU 5000-EXIT
           MOVE SPACES TO CA-MESSAGE
           MOVE VD-VAT-AMOUNT TO WS-MONEY-ED
           STRING "VAT at " FUNCTION TRIM(VD-LIABILITY) " is "
                  FUNCTION TRIM(WS-MONEY-ED) " "
                  FUNCTION TRIM(VD-CURRENCY)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2000-EXIT.
           EXIT.

       2100-ROUND-IT.
           INITIALIZE WS-RNDALLOC-CA
           MOVE "ROUND" TO RH-FUNCTION
           MOVE CA-COMPANY TO RH-COMPANY
           MOVE CA-USER TO RH-USER
           MOVE CA-BUSINESS-DATE TO RH-BUSINESS-DATE
           MOVE WS-BASIS TO RA-VALUE
           MOVE 2 TO RA-PLACES
           CALL "RNDALLOC" USING WS-RNDALLOC-CA
           IF RH-RETURN-CODE > 4
               COMPUTE WS-VAT ROUNDED = WS-BASIS
           ELSE
               MOVE RA-RESULT TO WS-VAT
           END-IF.
       2100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000 - A WHOLE DOCUMENT.  EACH LINE IS ROUNDED ON ITS OWN AND
      *>        THE TOTALS ARE THE SUM OF THE ROUNDED LINE VALUES.
      *>----------------------------------------------------------------
       3000-DOCUMENT.
           IF VD-LINE-CNT = 0
               MOVE "VAT0007" TO CA-REASON-CODE
               MOVE "No VAT lines were supplied" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           IF VD-LINE-CNT > 30
               MOVE 30 TO VD-LINE-CNT
           END-IF
           MOVE 0 TO VD-TOTAL-NET
           MOVE 0 TO VD-TOTAL-VAT
           PERFORM 3100-ONE-LINE THRU 3100-EXIT
               VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > VD-LINE-CNT OR CA-FAILED
           COMPUTE VD-TOTAL-GROSS = VD-TOTAL-NET + VD-TOTAL-VAT
           IF NOT CA-FAILED
               MOVE SPACES TO CA-MESSAGE
               MOVE VD-TOTAL-VAT TO WS-MONEY-ED
               STRING "Document VAT " FUNCTION TRIM(WS-MONEY-ED)
                      " on " FUNCTION TRIM(VD-VAT-CODE)
                      " and other rates, rounded line by line"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       3000-EXIT.
           EXIT.

       3100-ONE-LINE.
           MOVE VD-L-VAT-CODE(WS-IDX) TO VD-VAT-CODE
           MOVE VD-L-NET(WS-IDX) TO VD-NET-AMOUNT
           PERFORM 1000-RATE THRU 1000-EXIT
           IF CA-FAILED
               GO TO 3100-EXIT
           END-IF
           MOVE VD-RATE-PCT TO VD-L-RATE(WS-IDX)
           MOVE VD-L-NET(WS-IDX) TO WS-NET
           COMPUTE WS-BASIS = WS-NET * VD-RATE-PCT / 100
           PERFORM 2100-ROUND-IT THRU 2100-EXIT
           MOVE WS-VAT TO VD-L-VAT(WS-IDX)
           COMPUTE VD-L-GROSS(WS-IDX) = WS-NET + WS-VAT
           ADD WS-NET TO VD-TOTAL-NET
           ADD WS-VAT TO VD-TOTAL-VAT.
       3100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000 - SELF ACCOUNTING (REVERSE CHARGE, DRC, POSTPONED VAT)
      *>        THE SAME VALUE APPEARS AS OUTPUT AND AS INPUT TAX AND
      *>        NO CASH VAT IS PAID TO THE SUPPLIER (FIN-028).
      *>----------------------------------------------------------------
       4000-SELF-ACCOUNT.
           IF VD-MECHANISM = SPACES
               PERFORM 1000-RATE THRU 1000-EXIT
               IF CA-FAILED
                   GO TO 4000-EXIT
               END-IF
               COMPUTE WS-BASIS = VD-NET-AMOUNT * VD-RATE-PCT / 100
               PERFORM 2100-ROUND-IT THRU 2100-EXIT
               MOVE WS-VAT TO VD-VAT-AMOUNT
           END-IF
           MOVE 0 TO VD-SELF-OUTPUT-VAT
           MOVE 0 TO VD-SELF-INPUT-VAT
           IF VD-SELF-ACCOUNTS NOT = "Y"
               GO TO 4000-EXIT
           END-IF
           MOVE VD-VAT-AMOUNT TO VD-SELF-OUTPUT-VAT
           IF VD-RECOVERABLE = "Y"
               MOVE VD-VAT-AMOUNT TO VD-SELF-INPUT-VAT
           END-IF
           MOVE 1 TO VD-OUT-VAT-BOX
           MOVE 4 TO VD-IN-VAT-BOX
           IF VD-MECHANISM = "PVA"
               MOVE 0 TO VD-OUT-NET-BOX
               MOVE 7 TO VD-IN-NET-BOX
           END-IF
           IF CA-RETURN-CODE < 4
               MOVE 4 TO CA-RETURN-CODE
               MOVE "VAT0008" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               MOVE VD-SELF-OUTPUT-VAT TO WS-MONEY-ED
               STRING FUNCTION TRIM(WS-MONEY-ED)
                      " is self accounted as both output and input VAT"
                      " under " FUNCTION TRIM(VD-MECHANISM)
                      ". No VAT is paid to the supplier"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 5000 - WHICH VAT RETURN PERIOD DOES THE TAX POINT FALL IN?
      *>        A FINALISED PERIOD IS NOT REOPENED: THE ITEM GOES INTO
      *>        THE NEXT OPEN RETURN AS A LATE ENTRY (FIN-018).
      *>----------------------------------------------------------------
       5000-VAT-PERIOD.
           MOVE SPACES TO VD-VAT-PERIOD-KEY
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) p.VatPeriodKey, p.Status, "
                  "ISNULL(c.VatScheme,'INVOICE') FROM fin.VatPeriod p "
                  "JOIN core.Company c ON c.CompanyCode = "
                  "p.CompanyCode WHERE p.CompanyCode = ? "
                  "AND ? BETWEEN CONVERT(char(8), p.StartDate, 112) "
                  "AND CONVERT(char(8), p.EndDate, 112) "
                  "ORDER BY p.StartDate"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-DATE-TEXT TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 3 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 14 TO SQLIO-COL-WIDTH(2)
           MOVE 12 TO SQLIO-COL-WIDTH(3)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PER-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 5000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 4 TO CA-RETURN-CODE
               MOVE "VAT0010" TO CA-REASON-CODE
               MOVE "No VAT return period covers that tax point"
                   TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 5000-EXIT
           END-IF
           MOVE WS-E-SCHEME TO VD-SCHEME
           IF WS-E-STATUS = "FINALISED" OR WS-E-STATUS = "SUBMITTED"
             OR WS-E-STATUS = "AMENDED"
               MOVE "Y" TO VD-IS-LATE-ENTRY
               PERFORM 5100-NEXT-OPEN THRU 5100-EXIT
           ELSE
               MOVE WS-E-KEY TO VD-VAT-PERIOD-KEY
           END-IF.
       5000-EXIT.
           EXIT.

       5100-NEXT-OPEN.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) p.VatPeriodKey, p.Status, '' "
                  "FROM fin.VatPeriod p WHERE p.CompanyCode = ? "
                  "AND p.Status IN ('OPEN','PREPARING') "
                  "AND CONVERT(char(8), p.EndDate, 112) >= ? "
                  "ORDER BY p.StartDate"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-DATE-TEXT TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 3 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 14 TO SQLIO-COL-WIDTH(2)
           MOVE 12 TO SQLIO-COL-WIDTH(3)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PER-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 5100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "VAT0011" TO CA-REASON-CODE
               MOVE "The VAT return for that period is finalised and n"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5100-EXIT
           END-IF
           MOVE WS-E-KEY TO VD-VAT-PERIOD-KEY
           IF CA-RETURN-CODE < 4
               MOVE 4 TO CA-RETURN-CODE
               MOVE "VAT0012" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The return for that tax point is already "
                      "finalised, so the item is reported in "
                      FUNCTION TRIM(VD-VAT-PERIOD-KEY)
                      " as a late entry"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       5100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT.
           MOVE "Y" TO WS-REJECTED
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION.
           MOVE "VAT0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "VATDET does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF CA-MESSAGE = SPACES
               MOVE "SQL failure in VATDET" TO CA-MESSAGE
           END-IF
           MOVE 12 TO CA-RETURN-CODE
           MOVE "Y" TO WS-REJECTED
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
