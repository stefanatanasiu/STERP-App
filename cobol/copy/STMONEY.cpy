      *>================================================================
      *> STMONEY  - STERP shared money / quantity work areas
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : COMMON SERVICES
      *> PURPOSE  : Precision constants, scaling factors and edit masks
      *>            used by every program that handles sterling or
      *>            foreign amounts.  Spec 01 section 8:
      *>              money      - minor unit of the currency (GBP 2dp)
      *>              unit price - at least 6 decimals  (D-09 / COM-011)
      *>              working    - at least 12 decimals before rounding
      *>            Rounding is to nearest minor unit, exact halves away
      *>            from zero - see RNDALLOC function ROUND.
      *>            Compiles in fixed and free format (cols 8-72, *>).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-09-02 RJH  CR0233 DECIMAL CURRENCY WORK AREAS
      *> 2002-01-07 KMP  EURO FIELDS ADDED (NEVER ADOPTED, KEPT FOR AP)
      *> 2012-04-23 ANW  SIX DECIMAL UNIT PRICES, TWELVE DECIMAL WORK
      *> 2026-10-05 STERP POUND SIGN HELD AS X"A3" - SEE STRUTIL NOTES
      *>================================================================
      *> REQ: COM-011
       01  STM-MONEY-WORK.
      *> scaling factors 10**n, subscripted by (decimal places + 1)
           05  STM-FACTORS.
               10  FILLER              PIC 9(9) VALUE 1.
               10  FILLER              PIC 9(9) VALUE 10.
               10  FILLER              PIC 9(9) VALUE 100.
               10  FILLER              PIC 9(9) VALUE 1000.
               10  FILLER              PIC 9(9) VALUE 10000.
               10  FILLER              PIC 9(9) VALUE 100000.
               10  FILLER              PIC 9(9) VALUE 1000000.
           05  STM-FACTOR-TAB REDEFINES STM-FACTORS.
               10  STM-FACTOR          PIC 9(9) OCCURS 7 TIMES.
           05  STM-MAX-PLACES          PIC S9(4) VALUE 6.
           05  STM-MONEY-PLACES        PIC S9(4) VALUE 2.
           05  STM-PRICE-PLACES        PIC S9(4) VALUE 6.
           05  STM-QTY-PLACES          PIC S9(4) VALUE 6.
      *> working amounts - keep 12 decimals until a rounding boundary
           05  STM-WORK-AMT            PIC S9(13)V9(12) VALUE 0.
           05  STM-WORK-AMT2           PIC S9(13)V9(12) VALUE 0.
           05  STM-MONEY               PIC S9(13)V9(6) VALUE 0.
           05  STM-MONEY2              PIC S9(13)V9(6) VALUE 0.
           05  STM-QTY                 PIC S9(13)V9(6) VALUE 0.
           05  STM-PRICE               PIC S9(13)V9(6) VALUE 0.
           05  STM-UNITS               PIC S9(20) VALUE 0.
      *> display / report edit masks (COM-024 - explicit currency)
           05  STM-ED-MONEY            PIC -(12)9.99.
           05  STM-ED-MONEY-Z          PIC Z(9)9.99-.
           05  STM-ED-PRICE            PIC -(9)9.9(6).
           05  STM-ED-QTY              PIC -(9)9.9(6).
           05  STM-ED-RATE             PIC -(5)9.9(8).
           05  STM-ED-PCT              PIC -(3)9.9(4).
           05  STM-ED-INT              PIC -(9)9.
      *> the sterling symbol.  COBOL sources are single byte, so the
      *> pound is held as X"A3" (ISO-8859-1 / Windows-1252).  Messages
      *> returned in CA-MESSAGE use the text "GBP" instead - see
      *> docs\COMMON-SERVICES.md "Pound sign handling".
           05  STM-POUND               PIC X VALUE X"A3".
           05  STM-GBP-TEXT            PIC X(3) VALUE "GBP".
      *> D-22 tolerances (defaults - core.Parameter overrides them)
           05  STM-TOL-AR-WRITEOFF     PIC S9(7)V99 VALUE 5.00.
           05  STM-TOL-AP-DIFFERENCE   PIC S9(7)V99 VALUE 1.00.
           05  STM-TOL-FX-RESIDUAL     PIC S9(7)V99 VALUE 0.01.
