      *>================================================================
      *> PROGRAM-ID : AP6MDATE
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : THE SIX MONTH UNPAID INPUT VAT RULE IN ONE
      *>              PLACE.  INPUT VAT RECLAIMED ON A SUPPLIER
      *>              INVOICE MUST BE REPAID WHEN THE INVOICE IS STILL
      *>              UNPAID SIX MONTHS AFTER THE LATER OF ITS SUPPLY
      *>              DATE AND ITS DUE DATE, AND MAY BE RECLAIMED
      *>              AGAIN - ONCE - WHEN IT IS PAID.
      *>              SIX MONTHS IS A CALENDAR SHIFT, NOT 180 DAYS:
      *>              31 JANUARY 2027 BECOMES 31 JULY 2027 AND
      *>              31 AUGUST 2026 BECOMES 28 FEBRUARY 2027.
      *> CALLED BY  : APVAT6M, APALLOC, APVATEVD, TAPVAT6
      *> INTERFACE  : CALL "AP6MDATE" USING WS-6M-CA
      *>              COPYBOOK AP6MDTCA
      *> CALLS      : nothing - no database, no files
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2026-10-06 STERP AP-029 ORIGINAL
      *>================================================================
      *> REQ: AP-029, AP-025
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AP6MDATE.
       AUTHOR. STERP.
       DATE-WRITTEN. 06/10/2026.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK.
           05  WS-Y                    PIC S9(5) VALUE 0.
           05  WS-M                    PIC S9(5) VALUE 0.
           05  WS-D                    PIC S9(5) VALUE 0.
           05  WS-LAST-DAY             PIC S9(5) VALUE 0.
           05  WS-TOTAL-M              PIC S9(7) VALUE 0.
           05  WS-QTR                  PIC 9(1) VALUE 0.
           05  WS-INT-A                PIC S9(9) VALUE 0.
           05  WS-INT-B                PIC S9(9) VALUE 0.
           05  WS-RAW                  PIC S9(13)V9(6) VALUE 0.
           05  WS-PENCE                PIC S9(15) VALUE 0.
           05  WS-DATE-N               PIC 9(8) VALUE 0.
           05  WS-DATE-R REDEFINES WS-DATE-N.
               10  WS-DN-YYYY          PIC 9(4).
               10  WS-DN-MM            PIC 9(2).
               10  WS-DN-DD            PIC 9(2).
           05  WS-MONTH-TAB.
               10  FILLER              PIC 9(2) VALUE 31.
               10  FILLER              PIC 9(2) VALUE 28.
               10  FILLER              PIC 9(2) VALUE 31.
               10  FILLER              PIC 9(2) VALUE 30.
               10  FILLER              PIC 9(2) VALUE 31.
               10  FILLER              PIC 9(2) VALUE 30.
               10  FILLER              PIC 9(2) VALUE 31.
               10  FILLER              PIC 9(2) VALUE 31.
               10  FILLER              PIC 9(2) VALUE 30.
               10  FILLER              PIC 9(2) VALUE 31.
               10  FILLER              PIC 9(2) VALUE 30.
               10  FILLER              PIC 9(2) VALUE 31.
           05  WS-MONTH-R REDEFINES WS-MONTH-TAB.
               10  WS-MONTH-DAYS OCCURS 12 TIMES PIC 9(2).
       LINKAGE SECTION.
       COPY AP6MDTCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       0000-MAIN SECTION.
       0000-START.
           MOVE 0 TO SM-RC
           MOVE SPACES TO SM-REASON
           MOVE SPACES TO SM-MESSAGE
           EVALUATE TRUE
               WHEN SM-F-TRIGGER   PERFORM 1000-TRIGGER
               WHEN SM-F-REPAY     PERFORM 2000-REPAY
               WHEN SM-F-RECLAIM   PERFORM 3000-RECLAIM
               WHEN SM-F-PERIOD    PERFORM 4000-PERIOD
               WHEN OTHER
                   MOVE 08 TO SM-RC
                   MOVE "AP6M001" TO SM-REASON
                   MOVE "Unknown AP6MDATE function" TO SM-MESSAGE
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

      *>----------------------------------------------------------------
      *>   REQ: AP-029 - the clock starts at the LATER of the supply
      *>   date (tax point) and the due date, so extended credit does
      *>   not shorten the six months.
       1000-TRIGGER SECTION.
       1000-START.
           IF SM-MONTHS = 0
               MOVE 6 TO SM-MONTHS
           END-IF
           MOVE 0 TO SM-LATER-DATE
           IF SM-SUPPLY-DATE > SM-LATER-DATE
               MOVE SM-SUPPLY-DATE TO SM-LATER-DATE
           END-IF
           IF SM-DUE-DATE > SM-LATER-DATE
               MOVE SM-DUE-DATE TO SM-LATER-DATE
           END-IF
           IF SM-LATER-DATE = 0
               MOVE 08 TO SM-RC
               MOVE "AP6M002" TO SM-REASON
               MOVE "A supply date or a due date is required"
                   TO SM-MESSAGE
               MOVE 0 TO SM-TRIGGER-DATE
               MOVE "N" TO SM-DUE-FOR-REPAY
               GO TO 1000-EXIT
           END-IF
           MOVE SM-LATER-DATE TO WS-DATE-N
           PERFORM 1100-ADD-MONTHS
           MOVE WS-DATE-N TO SM-TRIGGER-DATE
           PERFORM 1200-COMPARE-AS-AT.
       1000-EXIT.
           EXIT.

      *>   a calendar month shift with the day clamped to the end of
      *>   the target month - 31 August plus six months is 28 (or 29)
      *>   February, never 3 March.
       1100-ADD-MONTHS SECTION.
       1100-START.
           MOVE WS-DN-YYYY TO WS-Y
           MOVE WS-DN-MM TO WS-M
           MOVE WS-DN-DD TO WS-D
           COMPUTE WS-TOTAL-M = (WS-Y * 12) + (WS-M - 1) + SM-MONTHS
           COMPUTE WS-Y = FUNCTION INTEGER (WS-TOTAL-M / 12)
           COMPUTE WS-M = WS-TOTAL-M - (WS-Y * 12) + 1
           IF WS-M > 12
               SUBTRACT 12 FROM WS-M
               ADD 1 TO WS-Y
           END-IF
           PERFORM 1150-LAST-DAY
           IF WS-D > WS-LAST-DAY
               MOVE WS-LAST-DAY TO WS-D
           END-IF
           COMPUTE WS-DATE-N = (WS-Y * 10000) + (WS-M * 100) + WS-D.
       1100-EXIT.
           EXIT.

       1150-LAST-DAY SECTION.
       1150-START.
           MOVE WS-MONTH-DAYS(WS-M) TO WS-LAST-DAY
           IF WS-M = 2
               IF FUNCTION MOD(WS-Y, 4) = 0
                  AND (FUNCTION MOD(WS-Y, 100) NOT = 0
                       OR FUNCTION MOD(WS-Y, 400) = 0)
                   MOVE 29 TO WS-LAST-DAY
               END-IF
           END-IF.
       1150-EXIT.
           EXIT.

      *>   the adjustment belongs to the return covering the trigger
      *>   date, so an as-at date on the trigger date itself is due.
       1200-COMPARE-AS-AT SECTION.
       1200-START.
           MOVE "N" TO SM-DUE-FOR-REPAY
           MOVE 0 TO SM-DAYS-OVER
           IF SM-AS-AT = 0
               GO TO 1200-EXIT
           END-IF
           COMPUTE WS-INT-A = FUNCTION INTEGER-OF-DATE(SM-AS-AT)
           COMPUTE WS-INT-B = FUNCTION INTEGER-OF-DATE(SM-TRIGGER-DATE)
           COMPUTE SM-DAYS-OVER = WS-INT-A - WS-INT-B
           IF SM-DAYS-OVER NOT < 0
               MOVE "Y" TO SM-DUE-FOR-REPAY
           END-IF
           MOVE SM-TRIGGER-DATE TO SM-PERIOD-DATE
           PERFORM 4000-PERIOD.
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   REQ: AP-029 - only the unpaid proportion is repaid, and a
      *>   previous repayment is never made twice.
       2000-REPAY SECTION.
       2000-START.
           PERFORM 1000-TRIGGER
           MOVE 0 TO SM-REPAY-VAT
           MOVE 0 TO SM-UNPAID-PCT
           IF SM-RC NOT < 08
               GO TO 2000-EXIT
           END-IF
           IF NOT SM-REPAYABLE
               GO TO 2000-EXIT
           END-IF
           IF SM-RESIDUAL NOT > 0 OR SM-CLAIMED-VAT NOT > 0
               MOVE "N" TO SM-DUE-FOR-REPAY
               GO TO 2000-EXIT
           END-IF
           IF SM-GROSS NOT > 0 OR SM-RESIDUAL NOT < SM-GROSS
               MOVE SM-CLAIMED-VAT TO SM-REPAY-VAT
               MOVE 100 TO SM-UNPAID-PCT
           ELSE
               COMPUTE SM-UNPAID-PCT ROUNDED =
                   (SM-RESIDUAL * 100) / SM-GROSS
               COMPUTE WS-RAW = (SM-CLAIMED-VAT * SM-RESIDUAL)
                                / SM-GROSS
               PERFORM 2100-ROUND-PENNY
               MOVE WS-RAW TO SM-REPAY-VAT
           END-IF
           IF SM-ALREADY-REPAID NOT = 0
               COMPUTE SM-REPAY-VAT = SM-REPAY-VAT - SM-ALREADY-REPAID
               IF SM-REPAY-VAT NOT > 0
                   MOVE 0 TO SM-REPAY-VAT
                   MOVE "N" TO SM-DUE-FOR-REPAY
                   MOVE 04 TO SM-RC
                   MOVE "AP6M003" TO SM-REASON
                   MOVE "Input VAT already repaid on this invoice"
                       TO SM-MESSAGE
               END-IF
           END-IF.
       2000-EXIT.
           EXIT.

      *>   half a penny rounds up, as D-02 requires
       2100-ROUND-PENNY SECTION.
       2100-START.
           COMPUTE WS-PENCE = FUNCTION INTEGER((WS-RAW * 100) + 0.5)
           COMPUTE WS-RAW = WS-PENCE / 100.
       2100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   REQ: AP-029 - the reclaim is the amount that was repaid and
      *>   happens in the return covering the payment date, once.
       3000-RECLAIM SECTION.
       3000-START.
           MOVE 0 TO SM-RECLAIM-VAT
           IF SM-ALREADY-REPAID NOT > 0
               MOVE 04 TO SM-RC
               MOVE "AP6M004" TO SM-REASON
               MOVE "Nothing was repaid, so nothing is reclaimable"
                   TO SM-MESSAGE
               GO TO 3000-EXIT
           END-IF
           IF SM-PAID-DATE = 0
               MOVE 08 TO SM-RC
               MOVE "AP6M005" TO SM-REASON
               MOVE "A payment date is required to reclaim"
                   TO SM-MESSAGE
               GO TO 3000-EXIT
           END-IF
           IF SM-RESIDUAL > 0
               COMPUTE WS-RAW = SM-ALREADY-REPAID
               IF SM-GROSS > 0
                   COMPUTE WS-RAW = (SM-ALREADY-REPAID *
                       (SM-GROSS - SM-RESIDUAL)) / SM-GROSS
               END-IF
               PERFORM 2100-ROUND-PENNY
               MOVE WS-RAW TO SM-RECLAIM-VAT
           ELSE
               MOVE SM-ALREADY-REPAID TO SM-RECLAIM-VAT
           END-IF
           MOVE SM-PAID-DATE TO SM-PERIOD-DATE
           PERFORM 4000-PERIOD.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   the ledger keeps calendar quarter keys (YYYYQn); a stagger
      *>   group would need the fin period table - see tech debt.
       4000-PERIOD SECTION.
       4000-START.
           MOVE SPACES TO SM-PERIOD-KEY
           IF SM-PERIOD-DATE = 0
               GO TO 4000-EXIT
           END-IF
           MOVE SM-PERIOD-DATE TO WS-DATE-N
           COMPUTE WS-QTR = FUNCTION INTEGER((WS-DN-MM + 2) / 3)
           STRING WS-DN-YYYY "Q" WS-QTR
               DELIMITED BY SIZE INTO SM-PERIOD-KEY.
       4000-EXIT.
           EXIT.
