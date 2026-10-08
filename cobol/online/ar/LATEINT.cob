      *>================================================================
      *> PROGRAM-ID : LATEINT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : STATUTORY INTEREST AND FIXED SUM COMPENSATION ON
      *>              OVERDUE QUALIFYING COMMERCIAL DEBTS UNDER THE
      *>              LATE PAYMENT OF COMMERCIAL DEBTS (INTEREST) ACT
      *>              1998.  INTEREST ACCRUES DAILY, SIMPLE, AT THE
      *>              REFERENCE RATE PLUS EIGHT PER CENT FROM THE DAY
      *>              AFTER THE DUE DATE.  COMPENSATION IS GBP 40, 70
      *>              OR 100 BY DEBT BAND.  NEITHER AMOUNT CARRIES VAT
      *>              AND THE CLAIM IS A SEPARATE DOCUMENT LINKED TO
      *>              THE ORIGINAL INVOICE.
      *> CALLED BY  : DUNNING BATCH, COLLECTION WORKLIST, .NET CLIENT
      *> INTERFACE  : CALL "LATEINT" USING WS-LATEINT-CA
      *>              COPYBOOK LATEINTCA
      *> CALLS      : SQLIO, PARMGET, PERMCHK, ARPOSTGL
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1999-04-06 RJH  CR1120 LATE PAYMENT ACT 1998 PHASE 1 -
      *>                 SMALL BUSINESS CREDITORS ONLY
      *> 2002-08-07 RJH  CR1455 ACT EXTENDED TO ALL BUSINESS DEBTS
      *> 2013-03-16 KMP  CR2001 FIXED SUM COMPENSATION 40/70/100
      *> 2026-10-05 STERP EFFECTIVE DATED BANK OF ENGLAND BASE RATE
      *>================================================================
      *> REQ: AR-024, D-25, D-28
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LATEINT.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 06/04/1999.
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
       01  WS-ITEM.
           05  WS-IT-ID                PIC X(24).
           05  WS-IT-CUSTOMER          PIC X(20).
           05  WS-IT-DOCNO             PIC X(30).
           05  WS-IT-DUE               PIC X(10).
           05  WS-IT-RESIDUAL          PIC X(24).
           05  WS-IT-DAYS              PIC X(12).
           05  WS-IT-FORM              PIC X(14).
           05  WS-IT-DISPUTED          PIC X(4).
       01  WS-ID-ROW.
           05  WS-NEW-ID               PIC X(24).
           05  WS-ID2                  PIC X(24).
       01  WS-RATE-BASE.
           05  WS-HALF                 PIC S9(2) VALUE 0.
           05  WS-YEAR                 PIC 9(4) VALUE 0.
           05  WS-MONTH                PIC 9(2) VALUE 0.
       01  WS-BANDS.
           05  WS-BAND1-LIMIT          PIC S9(9)V99 VALUE 1000.
           05  WS-BAND2-LIMIT          PIC S9(9)V99 VALUE 10000.
           05  WS-COMP1                PIC S9(9)V99 VALUE 40.
           05  WS-COMP2                PIC S9(9)V99 VALUE 70.
           05  WS-COMP3                PIC S9(9)V99 VALUE 100.
       LINKAGE SECTION.
       COPY LATEINTCA.
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
               WHEN "CALC"     PERFORM 1000-CALCULATE
               WHEN "CLAIM"    PERFORM 1000-CALCULATE
                               PERFORM 2000-RECORD-CLAIM
               WHEN "APPROVE"  PERFORM 3000-APPROVE-CLAIM
               WHEN "WAIVE"    PERFORM 3500-WAIVE-CLAIM
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
           MOVE CA-FUNCTION OF DFHCOMMAREA TO AR-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO AR-CO
           MOVE CA-SITE OF DFHCOMMAREA TO AR-SITE
           MOVE CA-USER OF DFHCOMMAREA TO AR-USERID
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO AR-BUSDATE
           MOVE "LATEINT" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO AR-RC AR-MSG-CNT
           MOVE SPACES TO AR-REASON AR-MSG
           MOVE "AR-024" TO AR-REQ-REF
           MOVE 0 TO LI-INTEREST LI-COMPENSATION LI-TOTAL-CLAIM
           MOVE 0 TO LI-COMP-BAND
           MOVE "N" TO LI-VAT-BEARING
           MOVE SPACES TO LI-EXPLANATION
           IF LI-CALC-TO-DATE = 0
               MOVE AR-BUSDATE TO LI-CALC-TO-DATE
           END-IF
           IF LI-MARGIN-PCT = 0
               MOVE 8 TO LI-MARGIN-PCT
           END-IF
           IF LI-QUALIFYING = SPACE OR LI-QUALIFYING = LOW-VALUE
               MOVE "Y" TO LI-QUALIFYING
           END-IF
           PERFORM 0150-READ-PARAMETERS.
       0100-EXIT.
           EXIT.

       0150-READ-PARAMETERS SECTION.
       0150-START.
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE AR-USERID TO CA-USER OF WS-PARM-CA
           MOVE LI-CALC-TO-DATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "LATEPAY.MARGIN.PCT" TO PM-PARAM-CODE
           MOVE "Y" TO PM-USE-DEFAULT
           MOVE "8" TO PM-DEFAULT-TEXT
           CALL "PARMGET" USING WS-PARM-CA
           IF PM-VALUE-NUM > 0
               MOVE PM-VALUE-NUM TO LI-MARGIN-PCT
           END-IF
           PERFORM 0160-BAND-1
           PERFORM 0170-BAND-2
           PERFORM 0180-BAND-3.
       0150-EXIT.
           EXIT.

       0160-BAND-1 SECTION.
       0160-START.
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE AR-USERID TO CA-USER OF WS-PARM-CA
           MOVE LI-CALC-TO-DATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "LATEPAY.COMP.BAND1" TO PM-PARAM-CODE
           MOVE "Y" TO PM-USE-DEFAULT
           MOVE "40" TO PM-DEFAULT-TEXT
           CALL "PARMGET" USING WS-PARM-CA
           IF PM-VALUE-NUM > 0
               MOVE PM-VALUE-NUM TO WS-COMP1
           END-IF.
       0160-EXIT.
           EXIT.

       0170-BAND-2 SECTION.
       0170-START.
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE AR-USERID TO CA-USER OF WS-PARM-CA
           MOVE LI-CALC-TO-DATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "LATEPAY.COMP.BAND2" TO PM-PARAM-CODE
           MOVE "Y" TO PM-USE-DEFAULT
           MOVE "70" TO PM-DEFAULT-TEXT
           CALL "PARMGET" USING WS-PARM-CA
           IF PM-VALUE-NUM > 0
               MOVE PM-VALUE-NUM TO WS-COMP2
           END-IF.
       0170-EXIT.
           EXIT.

       0180-BAND-3 SECTION.
       0180-START.
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE AR-USERID TO CA-USER OF WS-PARM-CA
           MOVE LI-CALC-TO-DATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "LATEPAY.COMP.BAND3" TO PM-PARAM-CODE
           MOVE "Y" TO PM-USE-DEFAULT
           MOVE "100" TO PM-DEFAULT-TEXT
           CALL "PARMGET" USING WS-PARM-CA
           IF PM-VALUE-NUM > 0
               MOVE PM-VALUE-NUM TO WS-COMP3
           END-IF.
       0180-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF AR-CO = SPACES
               MOVE "LPI0001" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A company code is required" DELIMITED BY SIZE
                   INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AR-FUNC = "WAIVE" AND LI-WAIVER-REASON = SPACES
               MOVE "LPI0002" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A waiver of statutory interest requires a "
                      "reason (D-28)"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-CALCULATE SECTION.
       1000-START.
           IF LI-OPEN-ITEM-ID NOT = 0 OR LI-INVOICE-NO NOT = SPACES
               PERFORM 1100-READ-OPEN-ITEM
               IF AR-RC-BAD
                   GO TO 1000-EXIT
               END-IF
           END-IF
           IF LI-PRINCIPAL <= 0
               MOVE 8 TO AR-RC
               MOVE "LPI0003" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "There is no outstanding principal to charge "
                      "interest on"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 1000-EXIT
           END-IF
           PERFORM 1200-DAYS-OVERDUE
           PERFORM 1300-REFERENCE-RATE
           IF AR-RC-BAD
               GO TO 1000-EXIT
           END-IF
           PERFORM 1400-INTEREST
           PERFORM 1500-COMPENSATION
           COMPUTE LI-TOTAL-CLAIM = LI-INTEREST + LI-COMPENSATION
                                  + LI-RECOVERY-COSTS
           MOVE "N" TO LI-VAT-BEARING
           PERFORM 1600-EXPLAIN.
       1000-EXIT.
           EXIT.

       1100-READ-OPEN-ITEM SECTION.
       1100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), o.OpenItemId), "
                  "o.CustomerCode, o.DocumentNumber, "
                  "CONVERT(char(8), o.DueDate, 112), "
                  "CONVERT(varchar(24), o.ResidualAmountGBP), "
                  "CONVERT(varchar(12), DATEDIFF(day, o.DueDate, "
                  "CONVERT(date, ?))), ISNULL((SELECT c.LegalForm "
                  "FROM ar.Customer c WHERE c.CompanyCode = "
                  "o.CompanyCode AND c.CustomerCode = o.CustomerCode)"
                  ",''), CONVERT(char(1), o.IsDisputed) "
                  "FROM ar.OpenItem o WHERE o.CompanyCode = ? "
                  "AND (o.OpenItemId = CASE WHEN ? = '0' THEN "
                  "o.OpenItemId ELSE CONVERT(bigint, ?) END) "
                  "AND (? = '' OR o.DocumentNumber = ?) "
                  "AND o.ItemType = 'INVOICE'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE LI-CALC-TO-DATE TO SQLIO-PARM-VAL(1)
           MOVE AR-CO TO SQLIO-PARM-VAL(2)
           MOVE LI-OPEN-ITEM-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE LI-INVOICE-NO TO SQLIO-PARM-VAL(5)
           MOVE LI-INVOICE-NO TO SQLIO-PARM-VAL(6)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 6
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE 8 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 30 TO SQLIO-COL-WIDTH(3)
           MOVE 10 TO SQLIO-COL-WIDTH(4)
           MOVE 24 TO SQLIO-COL-WIDTH(5)
           MOVE 12 TO SQLIO-COL-WIDTH(6)
           MOVE 14 TO SQLIO-COL-WIDTH(7)
           MOVE 4 TO SQLIO-COL-WIDTH(8)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ITEM
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "LPI0004" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "That invoice is not held in the open item "
                      "ledger" DELIMITED BY SIZE INTO AR-MSG
               GO TO 1100-EXIT
           END-IF
           COMPUTE LI-OPEN-ITEM-ID = FUNCTION NUMVAL(WS-IT-ID)
           MOVE WS-IT-CUSTOMER TO LI-CUSTOMER
           MOVE WS-IT-DOCNO TO LI-INVOICE-NO
           IF WS-IT-DUE(1:8) IS NUMERIC
               MOVE WS-IT-DUE(1:8) TO LI-DUE-DATE
           END-IF
           COMPUTE LI-PRINCIPAL = FUNCTION NUMVAL(WS-IT-RESIDUAL)
      *>   the Act applies to business to business debts; a consumer
      *>   debt is not a qualifying commercial debt
           IF WS-IT-FORM = "SOLETRADER" AND LI-ALT-REMEDY = "Y"
               MOVE "N" TO LI-QUALIFYING
           END-IF.
       1100-EXIT.
           EXIT.

      *>   interest runs from the day after the due date
       1200-DAYS-OVERDUE SECTION.
       1200-START.
           IF WS-IT-DAYS NOT = SPACES
               COMPUTE LI-DAYS-OVERDUE = FUNCTION NUMVAL(WS-IT-DAYS)
           END-IF
           IF LI-DAYS-OVERDUE < 0
               MOVE 0 TO LI-DAYS-OVERDUE
           END-IF.
       1200-EXIT.
           EXIT.

      *>   the reference rate is the Bank of England base rate on
      *>   31 December for interest arising 1 January to 30 June, or
      *>   on 30 June for 1 July to 31 December
       1300-REFERENCE-RATE SECTION.
       1300-START.
           IF LI-REF-RATE-PCT > 0
               MOVE LI-REF-RATE-PCT TO AR-PCT1
               GO TO 1350-COMBINE
           END-IF
           MOVE LI-CALC-TO-DATE TO AR-NUM-DATE
           MOVE AR-NUM-DATE TO AR-COMPACT-DATE
           MOVE AR-CMP-Y TO WS-YEAR
           MOVE AR-CMP-M TO WS-MONTH
           IF WS-MONTH <= 6
               COMPUTE WS-YEAR = WS-YEAR - 1
               MOVE 1 TO WS-HALF
               COMPUTE LI-RATE-BASIS-DATE = (WS-YEAR * 10000) + 1231
           ELSE
               MOVE 2 TO WS-HALF
               COMPUTE LI-RATE-BASIS-DATE = (WS-YEAR * 10000) + 630
           END-IF
           PERFORM 1320-READ-BOE-RATE
           IF AR-RC-BAD
               GO TO 1300-EXIT
           END-IF.
       1350-COMBINE.
           MOVE AR-PCT1 TO LI-REF-RATE-PCT
           COMPUTE LI-TOTAL-RATE-PCT =
               LI-REF-RATE-PCT + LI-MARGIN-PCT.
       1300-EXIT.
           EXIT.

       1320-READ-BOE-RATE SECTION.
       1320-START.
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE AR-USERID TO CA-USER OF WS-PARM-CA
           MOVE LI-RATE-BASIS-DATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "LATEPAY.BASE.RATE.PCT" TO PM-PARAM-CODE
           MOVE "Y" TO PM-USE-DEFAULT
           MOVE "4.00" TO PM-DEFAULT-TEXT
           CALL "PARMGET" USING WS-PARM-CA
           MOVE PM-VALUE-NUM TO AR-PCT1
           MOVE "BOE base rate" TO LI-RATE-SOURCE
           IF AR-PCT1 <= 0
               MOVE 4 TO AR-PCT1
               MOVE "BOE base rate (default)" TO LI-RATE-SOURCE
           END-IF.
       1320-EXIT.
           EXIT.

      *>   interest = principal x rate x days / 365, simple
      *>   AC-121: 5,000.00 x 12% x 30 / 365 = 49.32
       1400-INTEREST SECTION.
       1400-START.
           MOVE 0 TO LI-INTEREST
           IF LI-QUALIFYING NOT = "Y"
               MOVE 4 TO AR-RC
               MOVE "LPI0005" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The debt does not qualify for statutory "
                      "interest" DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 1400-EXIT
           END-IF
           IF LI-ALT-REMEDY = "Y"
               MOVE 4 TO AR-RC
               MOVE "LPI0006" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The contract provides a substantial "
                      "alternative remedy, so the Act does not apply"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 1400-EXIT
           END-IF
           IF LI-DAYS-OVERDUE <= 0
               GO TO 1400-EXIT
           END-IF
           COMPUTE AR-AMT1 =
               LI-PRINCIPAL * LI-TOTAL-RATE-PCT / 100
           COMPUTE AR-AMT2 ROUNDED =
               AR-AMT1 * LI-DAYS-OVERDUE / 365
           MOVE AR-AMT2 TO LI-INTEREST.
       1400-EXIT.
           EXIT.

      *>   GBP 40 under 1,000; GBP 70 from 1,000 to 9,999.99;
      *>   GBP 100 at 10,000 and above - per invoice
       1500-COMPENSATION SECTION.
       1500-START.
           MOVE 0 TO LI-COMPENSATION
           MOVE 0 TO LI-COMP-BAND
           IF LI-QUALIFYING NOT = "Y" OR LI-ALT-REMEDY = "Y"
               GO TO 1500-EXIT
           END-IF
           IF LI-DAYS-OVERDUE <= 0
               GO TO 1500-EXIT
           END-IF
           EVALUATE TRUE
               WHEN LI-PRINCIPAL < WS-BAND1-LIMIT
                   MOVE WS-COMP1 TO LI-COMPENSATION
                   MOVE 1 TO LI-COMP-BAND
               WHEN LI-PRINCIPAL < WS-BAND2-LIMIT
                   MOVE WS-COMP2 TO LI-COMPENSATION
                   MOVE 2 TO LI-COMP-BAND
               WHEN OTHER
                   MOVE WS-COMP3 TO LI-COMPENSATION
                   MOVE 3 TO LI-COMP-BAND
           END-EVALUATE.
       1500-EXIT.
           EXIT.

       1600-EXPLAIN SECTION.
       1600-START.
           MOVE LI-PRINCIPAL TO AR-MONEY-UK
           MOVE LI-TOTAL-RATE-PCT TO AR-MONEY-ED
           MOVE SPACES TO LI-EXPLANATION
           STRING "GBP " FUNCTION TRIM(AR-MONEY-UK) " x "
                  FUNCTION TRIM(AR-MONEY-ED) "% x "
               DELIMITED BY SIZE INTO LI-EXPLANATION
           MOVE LI-DAYS-OVERDUE TO AR-INT-ED
           MOVE LI-INTEREST TO AR-MONEY-ED
           MOVE SPACES TO AR-SCRATCH
           STRING FUNCTION TRIM(LI-EXPLANATION)
                  FUNCTION TRIM(AR-INT-ED) " / 365 = GBP "
                  FUNCTION TRIM(AR-MONEY-ED)
               DELIMITED BY SIZE INTO AR-SCRATCH
           MOVE AR-SCRATCH TO LI-EXPLANATION
           MOVE LI-COMPENSATION TO AR-MONEY-ED
           MOVE LI-TOTAL-CLAIM TO AR-MONEY-UK
           MOVE SPACES TO AR-MSG
           STRING "Statutory interest GBP "
                  FUNCTION TRIM(LI-EXPLANATION)
                  ", compensation GBP " FUNCTION TRIM(AR-MONEY-ED)
                  ", total GBP " FUNCTION TRIM(AR-MONEY-UK)
                  " - outside the scope of VAT"
               DELIMITED BY SIZE INTO AR-MSG.
       1600-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       2000-RECORD-CLAIM SECTION.
       2000-START.
           IF AR-RC-BAD
               GO TO 2000-EXIT
           END-IF
           IF LI-TOTAL-CLAIM <= 0
               MOVE 4 TO AR-RC
               MOVE "LPI0007" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "There is nothing to claim on this invoice"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 2000-EXIT
           END-IF
           IF LI-CLAIM-REF = SPACES
               MOVE SPACES TO LI-CLAIM-REF
               STRING "LPC-" FUNCTION TRIM(LI-INVOICE-NO)
                   DELIMITED BY SIZE INTO LI-CLAIM-REF
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; "
                  "DELETE ar.LatePaymentClaim WHERE CompanyCode = ? "
                  "AND ClaimRef = ? AND Status = 'CALC'; "
                  "INSERT ar.LatePaymentClaim (CompanyCode, "
                  "CustomerCode, ClaimRef, OpenItemId, InvoiceNumber,"
                  " InvoiceDueDate, PrincipalGBP, DaysOverdue, "
                  "ReferenceRatePct, MarginPct, InterestGBP, "
                  "CompensationGBP, RecoveryCostsGBP, TotalClaimGBP, "
                  "CalculatedTo, Status) VALUES (?, ?, ?, "
                  "NULLIF(?,0), ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, "
                  "'CALC'); "
                  "SELECT CONVERT(varchar(24), SCOPE_IDENTITY()), ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 17 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE LI-CLAIM-REF TO SQLIO-PARM-VAL(2)
           MOVE AR-CO TO SQLIO-PARM-VAL(3)
           MOVE LI-CUSTOMER TO SQLIO-PARM-VAL(4)
           MOVE LI-CLAIM-REF TO SQLIO-PARM-VAL(5)
           MOVE LI-OPEN-ITEM-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(6)
           MOVE LI-INVOICE-NO TO SQLIO-PARM-VAL(7)
           MOVE LI-DUE-DATE TO SQLIO-PARM-VAL(8)
           MOVE LI-PRINCIPAL TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(9)
           MOVE LI-DAYS-OVERDUE TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(10)
           MOVE LI-REF-RATE-PCT TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(11)
           MOVE LI-MARGIN-PCT TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(12)
           MOVE LI-INTEREST TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(13)
           MOVE LI-COMPENSATION TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(14)
           MOVE LI-RECOVERY-COSTS TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(15)
           MOVE LI-TOTAL-CLAIM TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(16)
           MOVE LI-CALC-TO-DATE TO SQLIO-PARM-VAL(17)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 17
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
           COMPUTE LI-CLAIM-ID = FUNCTION NUMVAL(WS-NEW-ID)
           MOVE "CALC" TO LI-STATUS
           MOVE SPACES TO AR-SCRATCH
           STRING FUNCTION TRIM(AR-MSG)
                  ". Claim " FUNCTION TRIM(LI-CLAIM-REF)
                  " awaits credit manager approval (D-28)"
               DELIMITED BY SIZE INTO AR-SCRATCH
           MOVE AR-SCRATCH TO AR-MSG
           MOVE 4 TO AR-RC
           MOVE "LPI0008" TO AR-REASON
           ADD 1 TO AR-MSG-CNT.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       3000-APPROVE-CLAIM SECTION.
       3000-START.
           PERFORM 3900-CHECK-AUTHORITY
           IF AR-RC-BAD
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.LatePaymentClaim SET Status = "
                  "'APPROVED', ApprovedBy = ?, ApprovedAt = "
                  "SYSDATETIME() WHERE CompanyCode = ? AND "
                  "ClaimRef = ? AND Status = 'CALC'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE AR-USERID TO SQLIO-PARM-VAL(1)
           MOVE AR-CO TO SQLIO-PARM-VAL(2)
           MOVE LI-CLAIM-REF TO SQLIO-PARM-VAL(3)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 3
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
           MOVE SQLIO-ROWCOUNT TO AR-ROWS
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF AR-ROWS = 0
               MOVE 16 TO AR-RC
               MOVE "LPI0009" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "No calculated claim "
                      FUNCTION TRIM(LI-CLAIM-REF) " awaits approval"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 3000-EXIT
           END-IF
           MOVE "APPROVED" TO LI-STATUS
           MOVE AR-USERID TO LI-APPROVER
           MOVE SPACES TO AR-MSG
           STRING "Late payment claim "
                  FUNCTION TRIM(LI-CLAIM-REF)
                  " approved and may be issued"
               DELIMITED BY SIZE INTO AR-MSG.
       3000-EXIT.
           EXIT.

       3500-WAIVE-CLAIM SECTION.
       3500-START.
           PERFORM 3900-CHECK-AUTHORITY
           IF AR-RC-BAD
               GO TO 3500-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.LatePaymentClaim SET Status = 'WAIVED', "
                  "WaivedBy = ?, WaiverReason = ? WHERE CompanyCode "
                  "= ? AND ClaimRef = ? AND Status IN ('CALC',"
                  "'APPROVED')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE AR-USERID TO SQLIO-PARM-VAL(1)
           MOVE LI-WAIVER-REASON TO SQLIO-PARM-VAL(2)
           MOVE AR-CO TO SQLIO-PARM-VAL(3)
           MOVE LI-CLAIM-REF TO SQLIO-PARM-VAL(4)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 4
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 3500-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "WAIVED" TO LI-STATUS
           MOVE SPACES TO AR-MSG
           STRING "Claim " FUNCTION TRIM(LI-CLAIM-REF)
                  " waived: " FUNCTION TRIM(LI-WAIVER-REASON)
               DELIMITED BY SIZE INTO AR-MSG.
       3500-EXIT.
           EXIT.

       3900-CHECK-AUTHORITY SECTION.
       3900-START.
           IF LI-CLAIM-REF = SPACES
               MOVE 8 TO AR-RC
               MOVE "LPI0010" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A claim reference is required" DELIMITED BY
                   SIZE INTO AR-MSG
               GO TO 3900-EXIT
           END-IF
           INITIALIZE WS-PERM-CA
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PERM-CA
           MOVE AR-SITE TO CA-SITE OF WS-PERM-CA
           MOVE AR-USERID TO CA-USER OF WS-PERM-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-PERM-CA
           MOVE "AR.LATEPAY.APPROVE" TO PK-PERMISSION
           CALL "PERMCHK" USING WS-PERM-CA
           IF CA-RETURN-CODE OF WS-PERM-CA > 4
               MOVE 6 TO AR-RC
               MOVE "LPI0011" TO AR-REASON
               MOVE CA-MESSAGE OF WS-PERM-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       3900-EXIT.
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
           MOVE "LPI0098" TO AR-REASON
           MOVE SPACES TO AR-MSG
           STRING "LATEINT does not support function ["
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

       9900-SET-RESULT SECTION.
       9900-START.
           MOVE AR-RC TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE AR-REASON TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE AR-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE AR-MSG-CNT TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE AR-REQ-REF TO CA-REQ-REF OF DFHCOMMAREA.
       9900-EXIT.
           EXIT.
