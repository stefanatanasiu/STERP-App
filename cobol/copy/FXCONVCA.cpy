      *>================================================================
      *> FXCONVCA - COMMAREA for FXCONV (currency conversion service)
      *> QUOTATION (D-14, spec 01 section 8)
      *>   Rates are FOREIGN CURRENCY UNITS PER ONE POUND STERLING,
      *>   for example US$1.25 = GBP 1.  Therefore
      *>       sterling = foreign / rate      foreign = sterling * rate
      *>   The direction is NEVER inferred from the magnitude of the
      *>   rate.  A caller that holds an inverse quotation says so in
      *>   FX-QUOTE-BASIS and must set FX-ALLOW-INVERT to "Y"; both the
      *>   entered quotation and the converted rate are then retained.
      *> CA-FUNCTION values
      *>   GETRATE   look up the rate for FX-CURRENCY / FX-RATE-TYPE
      *>             effective on FX-RATE-DATE (the latest published
      *>             rate on or before that date) -> FX-RATE,
      *>             FX-RATE-FOUND-DATE, FX-RATE-SOURCE
      *>   TOGBP     FX-FOREIGN-AMOUNT -> FX-GBP-AMOUNT
      *>   TOFCY     FX-GBP-AMOUNT     -> FX-FOREIGN-AMOUNT
      *>   REALISED  realised gain or loss between FX-RATE (the rate the
      *>             document was booked at) and FX-RATE-2 (the
      *>             settlement rate) for FX-FOREIGN-AMOUNT.
      *>             FX-POSITION ASSET (receivable, bank) or LIABILITY
      *>             (payable).  A positive FX-REALISED is a gain.
      *> TOGBP / TOFCY use FX-RATE when it is supplied and look the rate
      *>   up first when it is zero and FX-RATE-DATE is present.
      *> RETURN CODES 00 ok, 04 warning (rate inverted on request, rate
      *>   taken from an earlier date), 08 invalid (FX0001..FX0009),
      *>   16 no rate held.  FX-RESIDUAL carries the rounding residual
      *>   that D-22 allows to reach the rounding account.
      *>================================================================
      *> REQ: COM-010, COM-011   AC: AC-005
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  FX-BODY.
               10  FX-CURRENCY         PIC X(3).
               10  FX-RATE-DATE        PIC 9(8).
               10  FX-RATE-TYPE        PIC X(8).
               10  FX-RATE             PIC S9(9)V9(8) COMP-3.
               10  FX-RATE-ENTERED     PIC S9(9)V9(8) COMP-3.
               10  FX-QUOTE-BASIS      PIC X(10).
                   88  FX-FCY-PER-GBP            VALUE "FCYPERGBP".
                   88  FX-GBP-PER-FCY            VALUE "GBPPERFCY".
               10  FX-ALLOW-INVERT     PIC X.
               10  FX-RATE-SOURCE      PIC X(20).
               10  FX-RATE-FOUND-DATE  PIC 9(8).
               10  FX-FOREIGN-AMOUNT   PIC S9(13)V9(6) COMP-3.
               10  FX-GBP-AMOUNT       PIC S9(13)V9(6) COMP-3.
               10  FX-PLACES           PIC S9(4) SIGN LEADING SEPARATE.
               10  FX-RATE-2           PIC S9(9)V9(8) COMP-3.
               10  FX-RATE-2-DATE      PIC 9(8).
               10  FX-GBP-AMOUNT-2     PIC S9(13)V9(6) COMP-3.
               10  FX-REALISED         PIC S9(13)V9(6) COMP-3.
               10  FX-POSITION         PIC X(10).
                   88  FX-ASSET                  VALUE "ASSET".
                   88  FX-LIABILITY              VALUE "LIABILITY".
               10  FX-UNROUNDED        PIC S9(13)V9(12) COMP-3.
               10  FX-RESIDUAL         PIC S9(13)V9(6) COMP-3.
               10  FX-RATE-TEXT        PIC X(30).
