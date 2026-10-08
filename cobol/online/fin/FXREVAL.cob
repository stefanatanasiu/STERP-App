      *>================================================================
      *> PROGRAM-ID : FXREVAL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER
      *> PURPOSE    : FOREIGN CURRENCY CARRYING VALUES, PERIOD END
      *>              REVALUATION AND REALISED SETTLEMENT DIFFERENCES
      *>              (FIN-019, FIN-020).
      *>              THE ORIGINAL FOREIGN AMOUNT AND THE STERLING
      *>              AMOUNT AT THE APPROVED TRANSACTION RATE ARE BOTH
      *>              PRESERVED.  THE PERIOD END ADJUSTMENT IS
      *>              UNREALISED AND IS REVERSED AT THE START OF THE
      *>              NEXT PERIOD, SO A GAIN IS RECOGNISED EXACTLY
      *>              ONCE.  AC-016: US$1,200 AT 1.28 IS 937.50;
      *>              REVALUED AT 1.25 IT IS 960.00 (22.50 UNREALISED);
      *>              SETTLED AT 1.20 IT YIELDS 1,000.00 AND A
      *>              REALISED GAIN OF 62.50 - THE LIFETIME GAIN IS
      *>              62.50 AND NOT 85.00.
      *>              RATES ARE FOREIGN UNITS PER ONE POUND (D-14).
      *> CALLED BY  : REVALRUN, AR, AP, .NET CLIENT
      *> INTERFACE  : CALL "FXREVAL" USING WS-FXREVAL-CA
      *>              COPYBOOK FXREVALCA
      *> CALLS      : FXCONV, JNLPOST, PERMCHK, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-01-23 JMC CR0600 ORIGINAL - MICRO FOCUS COBOL/2
      *> 1999-01-04 KMP CR1010 EURO ADDED TO THE CURRENCY TABLE
      *> 2026-10-05 STERP REVALUATION REVERSAL, NO DOUBLE COUNTING
      *>================================================================
      *> REQ: FIN-019, FIN-020
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FXREVAL.
       AUTHOR. J M CLEARY.
       DATE-WRITTEN. 23/01/1997.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY FXCONVCA REPLACING ==DFHCOMMAREA== BY ==WS-FXCONV-CA==
                      LEADING ==CA-== BY ==FH-==.
       COPY JNLPOSTCA REPLACING ==DFHCOMMAREA== BY ==WS-JNLPOST-CA==
                      LEADING ==CA-== BY ==JH-==.
       01  WS-SWITCHES.
           05  WS-REJECTED             PIC X VALUE "N".
               88  WS-IS-REJECTED                VALUE "Y".
       01  WS-WORK.
           05  WS-IDX                  PIC S9(4) COMP-5 VALUE 0.
      *>   The rate date must be a full eight digit date.  It used to
      *>   be passed through WS-IDX, a binary halfword, which silently
      *>   truncated it, so FXCONV was never given a usable date and
      *>   no held rate was ever found (FIN-019).
           05  WS-RATE-DATE            PIC 9(8) VALUE 0.
           05  WS-LINE-NO              PIC 9(4) VALUE 0.
           05  WS-AMT                  PIC S9(13)V99 VALUE 0.
           05  WS-CORREL               PIC X(36).
           05  WS-DR-ACCOUNT           PIC X(12).
           05  WS-CR-ACCOUNT           PIC X(12).
           05  WS-MONEY-ED             PIC -(11)9.99.
       01  WS-BAL-ROW.
           05  WS-B-ACCOUNT            PIC X(12).
           05  WS-B-CURRENCY           PIC X(4).
           05  WS-B-FOREIGN            PIC X(22).
           05  WS-B-CARRYING           PIC X(22).
       01  WS-ID-ROW.
           05  WS-I-ID                 PIC X(18).
           05  WS-I-STATUS             PIC X(12).
           05  WS-I-JOURNAL            PIC X(30).
       LINKAGE SECTION.
       COPY FXREVALCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           EVALUATE CA-FUNCTION
               WHEN "CARRY"    PERFORM 1000-CARRY THRU 1000-EXIT
               WHEN "REVALUE"  PERFORM 2000-REVALUE THRU 2000-EXIT
               WHEN "REALISE"  PERFORM 3000-REALISE THRU 3000-EXIT
               WHEN "POST"     PERFORM 4000-POST-RUN THRU 4000-EXIT
               WHEN "REVERSE"  PERFORM 5000-REVERSE-RUN THRU 5000-EXIT
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.

       0100-INITIALISE.
           MOVE "N" TO WS-REJECTED
           MOVE "FXREVAL" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "FIN-020" TO CA-REQ-REF
           MOVE 0 TO FR-ORIGINAL-GBP
           MOVE 0 TO FR-REVALUED-GBP
           MOVE 0 TO FR-ADJUSTMENT-GBP
           MOVE 0 TO FR-SETTLED-GBP
           MOVE 0 TO FR-REALISED-GBP
           MOVE 0 TO FR-UNREALISED-GBP
           MOVE 0 TO FR-LIFETIME-GBP
           MOVE 0 TO FR-TOTAL-GAIN
           MOVE 0 TO FR-TOTAL-LOSS
           MOVE 0 TO FR-NET-ADJUSTMENT
           MOVE 0 TO FR-LINE-CNT
           MOVE SPACES TO FR-ADJUST-TYPE
           MOVE SPACES TO FR-GAIN-OR-LOSS
           MOVE SPACES TO FR-JOURNAL-NUMBER
           MOVE SPACES TO FR-REVERSAL-JNL
           MOVE SPACES TO FR-REVAL-STATUS
           IF FR-RATE-TYPE = SPACES
               MOVE "SPOT" TO FR-RATE-TYPE
           END-IF
           IF FR-AS-AT-DATE = 0
               MOVE CA-BUSINESS-DATE TO FR-AS-AT-DATE
           END-IF
           IF FR-POSITION = SPACES
               MOVE "ASSET" TO FR-POSITION
           END-IF
           MOVE FUNCTION UPPER-CASE(FR-CURRENCY) TO FR-CURRENCY.
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000 - CARRYING AMOUNT AT THE TRANSACTION RATE
      *>----------------------------------------------------------------
       1000-CARRY.
           IF FR-CURRENCY = SPACES OR FR-CURRENCY = "GBP"
               MOVE "FXR0001" TO CA-REASON-CODE
               MOVE "A foreign currency is required for a revaluation"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1000-EXIT
           END-IF
           IF FR-RATE-ORIGINAL = 0
               MOVE FR-AS-AT-DATE TO WS-RATE-DATE
               PERFORM 1100-GET-RATE THRU 1100-EXIT
               MOVE FX-RATE TO FR-RATE-ORIGINAL
           END-IF
           IF FR-RATE-ORIGINAL = 0
               MOVE "FXR0002" TO CA-REASON-CODE
               MOVE "No positive exchange rate is held, so the sterlin"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1000-EXIT
           END-IF
           COMPUTE FR-ORIGINAL-GBP ROUNDED =
               FR-FOREIGN-AMOUNT / FR-RATE-ORIGINAL
           IF FR-CARRYING-GBP = 0
               MOVE FR-ORIGINAL-GBP TO FR-CARRYING-GBP
           END-IF
           MOVE SPACES TO CA-MESSAGE
           MOVE FR-ORIGINAL-GBP TO WS-MONEY-ED
           STRING FUNCTION TRIM(FR-CURRENCY) " carried at "
                  FUNCTION TRIM(WS-MONEY-ED)
                  " GBP at the transaction rate"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       1000-EXIT.
           EXIT.

       1100-GET-RATE.
           INITIALIZE WS-FXCONV-CA
           MOVE "GETRATE" TO FH-FUNCTION
           MOVE CA-COMPANY TO FH-COMPANY
           MOVE CA-USER TO FH-USER
           MOVE CA-BUSINESS-DATE TO FH-BUSINESS-DATE
           MOVE FR-CURRENCY TO FX-CURRENCY
           MOVE WS-RATE-DATE TO FX-RATE-DATE
           MOVE FR-RATE-TYPE TO FX-RATE-TYPE
           MOVE "FCYPERGBP" TO FX-QUOTE-BASIS
           MOVE "N" TO FX-ALLOW-INVERT
           CALL "FXCONV" USING WS-FXCONV-CA
           IF FH-RETURN-CODE > 4
               MOVE FH-REASON-CODE TO CA-REASON-CODE
               MOVE FH-MESSAGE TO CA-MESSAGE
               MOVE FH-RETURN-CODE TO CA-RETURN-CODE
               MOVE 0 TO FX-RATE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000 - PERIOD END REVALUATION (UNREALISED)
      *>----------------------------------------------------------------
       2000-REVALUE.
           PERFORM 1000-CARRY THRU 1000-EXIT
           IF CA-FAILED
               GO TO 2000-EXIT
           END-IF
           IF FR-RATE-AT-DATE = 0
               MOVE FR-AS-AT-DATE TO WS-RATE-DATE
               PERFORM 1100-GET-RATE THRU 1100-EXIT
               MOVE FX-RATE TO FR-RATE-AT-DATE
           END-IF
           IF FR-RATE-AT-DATE = 0
               MOVE "FXR0003" TO CA-REASON-CODE
               MOVE "No closing rate is held for that revaluation date"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           COMPUTE FR-REVALUED-GBP ROUNDED =
               FR-FOREIGN-AMOUNT / FR-RATE-AT-DATE
           COMPUTE FR-ADJUSTMENT-GBP =
               FR-REVALUED-GBP - FR-CARRYING-GBP
           MOVE FR-ADJUSTMENT-GBP TO FR-UNREALISED-GBP
           MOVE "UNREALISED" TO FR-ADJUST-TYPE
           IF FR-POSITION = "LIABILITY"
      *>       A liability revalued upwards is a loss, not a gain.
               COMPUTE FR-UNREALISED-GBP = 0 - FR-ADJUSTMENT-GBP
           END-IF
           IF FR-UNREALISED-GBP < 0
               MOVE "LOSS" TO FR-GAIN-OR-LOSS
               COMPUTE FR-TOTAL-LOSS = 0 - FR-UNREALISED-GBP
           ELSE
               MOVE "GAIN" TO FR-GAIN-OR-LOSS
               MOVE FR-UNREALISED-GBP TO FR-TOTAL-GAIN
           END-IF
           MOVE FR-UNREALISED-GBP TO FR-NET-ADJUSTMENT
           MOVE SPACES TO CA-MESSAGE
           MOVE FR-REVALUED-GBP TO WS-MONEY-ED
           STRING "Carrying value at the closing rate is "
                  FUNCTION TRIM(WS-MONEY-ED)
                  " GBP. The adjustment is unrealised and is reversed "
                  "at the start of the next period"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000 - REALISED GAIN OR LOSS ON SETTLEMENT
      *>        SETTLEMENT REMOVES THE CARRYING AMOUNT; ANY EARLIER
      *>        REVALUATION HAS ALREADY BEEN REVERSED, SO THE LIFETIME
      *>        FIGURE IS NOT DOUBLE COUNTED (FIN-020).
      *>----------------------------------------------------------------
       3000-REALISE.
           PERFORM 1000-CARRY THRU 1000-EXIT
           IF CA-FAILED
               GO TO 3000-EXIT
           END-IF
           IF FR-RATE-SETTLE = 0
               MOVE FR-AS-AT-DATE TO WS-RATE-DATE
               PERFORM 1100-GET-RATE THRU 1100-EXIT
               MOVE FX-RATE TO FR-RATE-SETTLE
           END-IF
           IF FR-RATE-SETTLE = 0
               MOVE "FXR0004" TO CA-REASON-CODE
               MOVE "No settlement rate is held for that date"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           COMPUTE FR-SETTLED-GBP ROUNDED =
               FR-FOREIGN-AMOUNT / FR-RATE-SETTLE
           COMPUTE FR-REALISED-GBP =
               FR-SETTLED-GBP - FR-ORIGINAL-GBP
           IF FR-POSITION = "LIABILITY"
               COMPUTE FR-REALISED-GBP =
                   FR-ORIGINAL-GBP - FR-SETTLED-GBP
           END-IF
           MOVE "REALISED" TO FR-ADJUST-TYPE
           MOVE FR-REALISED-GBP TO FR-LIFETIME-GBP
           IF FR-REALISED-GBP < 0
               MOVE "LOSS" TO FR-GAIN-OR-LOSS
               COMPUTE FR-TOTAL-LOSS = 0 - FR-REALISED-GBP
           ELSE
               MOVE "GAIN" TO FR-GAIN-OR-LOSS
               MOVE FR-REALISED-GBP TO FR-TOTAL-GAIN
           END-IF
           MOVE FR-REALISED-GBP TO FR-NET-ADJUSTMENT
           MOVE SPACES TO CA-MESSAGE
           MOVE FR-REALISED-GBP TO WS-MONEY-ED
           STRING "Realised exchange "
                  FUNCTION TRIM(FUNCTION LOWER-CASE(FR-GAIN-OR-LOSS))
                  " of " FUNCTION TRIM(WS-MONEY-ED)
                  " GBP on settlement. The lifetime difference is "
                  "recognised once"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000 - POST A REVALUATION RUN AND RECORD IT
      *>----------------------------------------------------------------
       4000-POST-RUN.
           PERFORM 2000-REVALUE THRU 2000-EXIT
           IF CA-FAILED
               GO TO 4000-EXIT
           END-IF
           IF FR-ACCOUNT = SPACES
               MOVE "FXR0005" TO CA-REASON-CODE
               MOVE "The monetary account being revalued is required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           IF FR-UNREALISED-GBP = 0
               MOVE 4 TO CA-RETURN-CODE
               MOVE "FXR0006" TO CA-REASON-CODE
               MOVE "The closing rate produces no adjustment, so no jo"
                   TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 4000-EXIT
           END-IF
           PERFORM 4100-WRITE-RUN THRU 4100-EXIT
           IF CA-FAILED
               GO TO 4000-EXIT
           END-IF
           MOVE SPACES TO WS-CORREL
           STRING "FXR-" FUNCTION TRIM(CA-COMPANY) "-"
                  FR-AS-AT-DATE "-" FUNCTION TRIM(FR-ACCOUNT) "-"
                  FUNCTION TRIM(FR-CURRENCY)
               DELIMITED BY SIZE INTO WS-CORREL
           IF FR-UNREALISED-GBP > 0
               MOVE FR-ACCOUNT TO WS-DR-ACCOUNT
               MOVE "7110" TO WS-CR-ACCOUNT
               MOVE FR-UNREALISED-GBP TO WS-AMT
           ELSE
               MOVE "7110" TO WS-DR-ACCOUNT
               MOVE FR-ACCOUNT TO WS-CR-ACCOUNT
               COMPUTE WS-AMT = 0 - FR-UNREALISED-GBP
           END-IF
           PERFORM 4200-STAGE-PAIR THRU 4200-EXIT
           IF CA-FAILED
               GO TO 4000-EXIT
           END-IF
           PERFORM 4300-POST THRU 4300-EXIT
           IF CA-FAILED
               GO TO 4000-EXIT
           END-IF
           PERFORM 4400-MARK-POSTED THRU 4400-EXIT
           MOVE "POSTED" TO FR-REVAL-STATUS
           MOVE SPACES TO CA-MESSAGE
           MOVE FR-UNREALISED-GBP TO WS-MONEY-ED
           STRING "Revaluation journal "
                  FUNCTION TRIM(FR-JOURNAL-NUMBER) " posted for "
                  FUNCTION TRIM(WS-MONEY-ED)
                  " GBP unrealised. It must be reversed in the next "
                  "period"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       4000-EXIT.
           EXIT.

       4100-WRITE-RUN.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.FxRevaluation (CompanyCode, FiscalYear, "
                  "PeriodNo, AsAtDate, RateType, TotalGainGBP, "
                  "TotalLossGBP, NetAdjustmentGBP, Status, RunBy) "
                  "OUTPUT inserted.RevalId, inserted.Status, '' "
                  "SELECT ?, ?, ?, ?, ?, ?, ?, ?, 'PREVIEW', ? "
                  "WHERE NOT EXISTS (SELECT 1 FROM fin.FxRevaluation x"
                  " WHERE x.CompanyCode = ? AND x.FiscalYear = ? "
                  "AND x.PeriodNo = ? AND x.AsAtDate = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 13 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE FR-FISCAL-YEAR TO SQLIO-PARM-VAL(2)
           MOVE FR-PERIOD-NO TO SQLIO-PARM-VAL(3)
           MOVE FR-AS-AT-DATE TO SQLIO-PARM-VAL(4)
           MOVE FR-RATE-TYPE TO SQLIO-PARM-VAL(5)
           MOVE FR-TOTAL-GAIN TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE FR-TOTAL-LOSS TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE FR-NET-ADJUSTMENT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE CA-USER TO SQLIO-PARM-VAL(9)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(10)
           MOVE FR-FISCAL-YEAR TO SQLIO-PARM-VAL(11)
           MOVE FR-PERIOD-NO TO SQLIO-PARM-VAL(12)
           MOVE FR-AS-AT-DATE TO SQLIO-PARM-VAL(13)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 3 TO SQLIO-COL-CNT
           MOVE 18 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 30 TO SQLIO-COL-WIDTH(3)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 4100-EXIT
           END-IF
           IF NOT SQLIO-NOTFOUND
               COMPUTE FR-REVAL-ID = FUNCTION NUMVAL(WS-I-ID)
           END-IF
           IF FR-REVAL-ID = 0
               PERFORM 4150-FIND-RUN THRU 4150-EXIT
           END-IF.
       4100-EXIT.
           EXIT.

       4150-FIND-RUN.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(x.RevalId AS varchar(18)), x.Status, "
                  "ISNULL(x.JournalNumber,'') FROM fin.FxRevaluation x"
                  " WHERE x.CompanyCode = ? AND x.FiscalYear = ? "
                  "AND x.PeriodNo = ? AND x.AsAtDate = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE FR-FISCAL-YEAR TO SQLIO-PARM-VAL(2)
           MOVE FR-PERIOD-NO TO SQLIO-PARM-VAL(3)
           MOVE FR-AS-AT-DATE TO SQLIO-PARM-VAL(4)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 3 TO SQLIO-COL-CNT
           MOVE 18 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 30 TO SQLIO-COL-WIDTH(3)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 4150-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 4150-EXIT
           END-IF
           COMPUTE FR-REVAL-ID = FUNCTION NUMVAL(WS-I-ID)
           MOVE WS-I-STATUS TO FR-REVAL-STATUS
           MOVE WS-I-JOURNAL TO FR-JOURNAL-NUMBER.
       4150-EXIT.
           EXIT.

       4200-STAGE-PAIR.
           MOVE 1 TO WS-LINE-NO
           PERFORM 4250-STAGE-ONE THRU 4250-EXIT
           MOVE 2 TO WS-LINE-NO
           PERFORM 4250-STAGE-ONE THRU 4250-EXIT.
       4200-EXIT.
           EXIT.

       4250-STAGE-ONE.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.JournalStage (CorrelationId, LineNum, "
                  "CompanyCode, AccountCode, DebitAmount, "
                  "CreditAmount, CurrencyCode, DimCostCentre, "
                  "Narrative, SubledgerType, SubledgerRef, CreatedBy) "
                  "SELECT ?, ?, ?, ?, ?, ?, 'GBP', NULLIF(?,''), ?, "
                  "NULLIF(?,''), NULLIF(?,''), ? WHERE NOT EXISTS "
                  "(SELECT 1 FROM fin.JournalStage s WHERE "
                  "s.CorrelationId = ? AND s.LineNum = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 13 TO SQLIO-PARM-CNT
           MOVE WS-CORREL TO SQLIO-PARM-VAL(1)
           MOVE WS-LINE-NO TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           IF WS-LINE-NO = 1
               MOVE WS-DR-ACCOUNT TO SQLIO-PARM-VAL(4)
               MOVE WS-AMT TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
               MOVE "0" TO SQLIO-PARM-VAL(6)
           ELSE
               MOVE WS-CR-ACCOUNT TO SQLIO-PARM-VAL(4)
               MOVE "0" TO SQLIO-PARM-VAL(5)
               MOVE WS-AMT TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           END-IF
           IF SQLIO-PARM-VAL(4)(1:4) = "7110"
               MOVE "ADMIN" TO SQLIO-PARM-VAL(7)
           ELSE
               MOVE SPACES TO SQLIO-PARM-VAL(7)
           END-IF
           MOVE SPACES TO SQLIO-PARM-VAL(8)
           STRING "Period end revaluation of "
                  FUNCTION TRIM(FR-CURRENCY) " balances"
               DELIMITED BY SIZE INTO SQLIO-PARM-VAL(8)
           MOVE FR-SUBLEDGER-TYPE TO SQLIO-PARM-VAL(9)
           MOVE FR-SUBLEDGER-REF TO SQLIO-PARM-VAL(10)
           MOVE CA-USER TO SQLIO-PARM-VAL(11)
           MOVE WS-CORREL TO SQLIO-PARM-VAL(12)
           MOVE WS-LINE-NO TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(13)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       4250-EXIT.
           EXIT.

       4300-POST.
           INITIALIZE WS-JNLPOST-CA
           MOVE "POST" TO JH-FUNCTION
           MOVE CA-COMPANY TO JH-COMPANY
           MOVE CA-SITE TO JH-SITE
           MOVE CA-USER TO JH-USER
           MOVE CA-BUSINESS-DATE TO JH-BUSINESS-DATE
           MOVE WS-CORREL TO JH-CORRELATION-ID
           MOVE FR-AS-AT-DATE TO JP-POSTING-DATE
           MOVE FR-AS-AT-DATE TO JP-DOCUMENT-DATE
           MOVE "GBP" TO JP-CURRENCY
           MOVE "FIN" TO JP-SOURCE-MODULE
           MOVE FR-CURRENCY TO JP-SOURCE-REF
           MOVE "FIN.REVALUATION" TO JP-EVENT-CODE
           MOVE "REVAL" TO JP-JOURNAL-TYPE
           MOVE "JNL" TO JP-SERIES-CODE
           MOVE SPACES TO JP-DESCRIPTION
           STRING "Unrealised revaluation of "
                  FUNCTION TRIM(FR-CURRENCY) " monetary balances"
               DELIMITED BY SIZE INTO JP-DESCRIPTION
           MOVE "fin.FxRevaluation" TO JP-EVIDENCE-REF
           CALL "JNLPOST" USING WS-JNLPOST-CA
           MOVE JP-NEW-JOURNAL-NO TO FR-JOURNAL-NUMBER
           IF JH-RETURN-CODE > 4
               MOVE JH-REASON-CODE TO CA-REASON-CODE
               MOVE JH-MESSAGE TO CA-MESSAGE
               MOVE JH-RETURN-CODE TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       4300-EXIT.
           EXIT.

       4400-MARK-POSTED.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.FxRevaluation SET Status = 'POSTED', "
                  "JournalNumber = ?, CorrelationId = ?, "
                  "TotalGainGBP = ?, TotalLossGBP = ?, "
                  "NetAdjustmentGBP = ? WHERE RevalId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE FR-JOURNAL-NUMBER TO SQLIO-PARM-VAL(1)
           MOVE WS-CORREL TO SQLIO-PARM-VAL(2)
           MOVE FR-TOTAL-GAIN TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE FR-TOTAL-LOSS TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE FR-NET-ADJUSTMENT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE FR-REVAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.FxRevaluationLine (RevalId, CompanyCode,"
                  " AccountCode, SubledgerType, SubledgerRef, "
                  "CurrencyCode, ForeignBalance, CarryingAmountGBP, "
                  "RateOriginal, RateAtDate, RevaluedAmountGBP, "
                  "AdjustmentGBP, AdjustmentType) VALUES (?, ?, ?, "
                  "NULLIF(?,''), NULLIF(?,''), ?, ?, ?, ?, ?, ?, ?, "
                  "'UNREALISED')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 12 TO SQLIO-PARM-CNT
           MOVE FR-REVAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
           MOVE FR-ACCOUNT TO SQLIO-PARM-VAL(3)
           MOVE FR-SUBLEDGER-TYPE TO SQLIO-PARM-VAL(4)
           MOVE FR-SUBLEDGER-REF TO SQLIO-PARM-VAL(5)
           MOVE FR-CURRENCY TO SQLIO-PARM-VAL(6)
           MOVE FR-FOREIGN-AMOUNT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE FR-CARRYING-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE FR-RATE-ORIGINAL TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE FR-RATE-AT-DATE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE FR-REVALUED-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE FR-UNREALISED-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       4400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 5000 - REVERSE THE REVALUATION ON DAY ONE OF THE NEXT PERIOD
      *>----------------------------------------------------------------
       5000-REVERSE-RUN.
           PERFORM 4150-FIND-RUN THRU 4150-EXIT
           IF FR-REVAL-ID = 0
               MOVE "FXR0010" TO CA-REASON-CODE
               MOVE "No revaluation run is held for that period"
                   TO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 5000-EXIT
           END-IF
           IF FR-REVAL-STATUS = "REVERSED"
               MOVE 4 TO CA-RETURN-CODE
               MOVE "FXR0011" TO CA-REASON-CODE
               MOVE "That revaluation has already been reversed exactl"
                   TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 5000-EXIT
           END-IF
           IF FR-REVAL-STATUS NOT = "POSTED"
               MOVE "FXR0012" TO CA-REASON-CODE
               MOVE "Only a posted revaluation can be reversed"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO WS-CORREL
           STRING "FXV-" FUNCTION TRIM(CA-COMPANY) "-"
                  FUNCTION TRIM(FR-JOURNAL-NUMBER)
               DELIMITED BY SIZE INTO WS-CORREL
           INITIALIZE WS-JNLPOST-CA
           MOVE "REVERSE" TO JH-FUNCTION
           MOVE CA-COMPANY TO JH-COMPANY
           MOVE CA-SITE TO JH-SITE
           MOVE CA-USER TO JH-USER
           MOVE CA-BUSINESS-DATE TO JH-BUSINESS-DATE
           MOVE WS-CORREL TO JH-CORRELATION-ID
           MOVE FR-JOURNAL-NUMBER TO JP-JOURNAL-NUMBER
           MOVE FR-AS-AT-DATE TO JP-POSTING-DATE
           MOVE 100 TO JP-REVERSE-PCT
           MOVE "Reversal of the period end revaluation" TO JP-REASON
           CALL "JNLPOST" USING WS-JNLPOST-CA
           MOVE JP-NEW-JOURNAL-NO TO FR-REVERSAL-JNL
           IF JH-RETURN-CODE > 4
               MOVE JH-REASON-CODE TO CA-REASON-CODE
               MOVE JH-MESSAGE TO CA-MESSAGE
               MOVE JH-RETURN-CODE TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.FxRevaluation SET Status = 'REVERSED', "
                  "ReversalJournalNo = ? WHERE RevalId = ? "
                  "AND Status = 'POSTED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE FR-REVERSAL-JNL TO SQLIO-PARM-VAL(1)
           MOVE FR-REVAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 5000-EXIT
           END-IF
           MOVE "REVERSED" TO FR-REVAL-STATUS
           MOVE SPACES TO CA-MESSAGE
           STRING "Revaluation reversed as journal "
                  FUNCTION TRIM(FR-REVERSAL-JNL)
                  ". The carrying value returns to the transaction "
                  "rate until settlement"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       5000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-NOT-NULL.
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(WS-IDX)
           END-PERFORM.
       8000-EXIT.
           EXIT.

       9000-REJECT.
           MOVE "Y" TO WS-REJECTED
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION.
           MOVE "FXR0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "FXREVAL does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF CA-MESSAGE = SPACES
               MOVE "SQL failure in FXREVAL" TO CA-MESSAGE
           END-IF
           MOVE 12 TO CA-RETURN-CODE
           MOVE "Y" TO WS-REJECTED
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
