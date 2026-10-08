      *>================================================================
      *> APBATWK - shared working storage for the purchase ledger batch
      *> SYSTEM  : STERP - Stefan's ERP
      *> MODULE  : CREDITORS AND SUPPLIER SETTLEMENT
      *>
      *> The 1991 purchase ledger batch suite fetches every column into
      *> a 100 character slot and converts on the way out, exactly as
      *> the stock control suite does.  It hides the real column types
      *> but a new listing needs no new copybook.  See
      *> docs\tech-debt\ap.md.
      *>
      *> Usage
      *>     COPY APBATWK.
      *>     MOVE 9 TO SQLIO-COL-CNT
      *>     PERFORM AP-WIDTHS
      *>     ...  CALL "SQLIO" USING SQLIO-CA AP-ROW
      *>     COMPUTE WS-AMT = FUNCTION NUMVAL(AP-COL(4))
      *>----------------------------------------------------------------
      *> 1991-02-08 RJH  CR0101 ORIGINAL
      *> 1999-10-04 KMP  Y2K - WS-DATE-N WIDENED TO 8
      *> 2026-10-06 STERP AP-015 BACS AND RPT-018 WORK FIELDS
      *>================================================================
       01  AP-ROW.
           05  AP-COL OCCURS 24 TIMES  PIC X(100).
       01  AP-SCRATCH.
           05  WS-IX                   PIC S9(4) VALUE 0.
           05  WS-JX                   PIC S9(4) VALUE 0.
           05  WS-KX                   PIC S9(4) VALUE 0.
           05  WS-ROWS                 PIC S9(9) VALUE 0.
           05  WS-AMT                  PIC S9(13)V99 VALUE 0.
           05  WS-AMT2                 PIC S9(13)V99 VALUE 0.
           05  WS-QTY                  PIC S9(13)V9(6) VALUE 0.
           05  WS-RATE                 PIC S9(9)V9(8) VALUE 1.
           05  WS-PCT                  PIC S9(7)V9(4) VALUE 0.
           05  WS-DAYS                 PIC S9(7) VALUE 0.
           05  WS-TEXT                 PIC X(200) VALUE SPACES.
           05  WS-TEXT2                PIC X(200) VALUE SPACES.
           05  WS-CODE                 PIC X(40) VALUE SPACES.
           05  WS-FLAG                 PIC X VALUE "N".
           05  WS-SQL-FAILED           PIC X VALUE "N".
           05  WS-DONE                 PIC X VALUE "N".
           05  WS-DATE-TEXT            PIC X(10) VALUE SPACES.
           05  WS-DATE-N               PIC 9(8) VALUE 0.
           05  WS-DATE-R REDEFINES WS-DATE-N.
               10  WS-DATE-YYYY        PIC 9(4).
               10  WS-DATE-MM          PIC 9(2).
               10  WS-DATE-DD          PIC 9(2).
           05  WS-TODAY-N              PIC 9(8) VALUE 0.
           05  WS-NUM-ED               PIC -(13)9.9(6).
           05  WS-INT-ED               PIC -(9)9.
           05  WS-MONEY-ED             PIC -(11)9.99.
           05  WS-MONEY-Z              PIC Z(9)9.99-.
           05  WS-PCT-ED               PIC Z(2)9.9.
      *>   an ODBC date (YYYY-MM-DD) converted to the YYYYMMDD number
      *>   the COMMAREAs carry - AP-DATE-CONV does the shuffling
       01  AP-DATE-CONV.
           05  AP-DC-TEXT              PIC X(10) VALUE SPACES.
           05  AP-DC-PARTS REDEFINES AP-DC-TEXT.
               10  AP-DC-YYYY          PIC 9(4).
               10  FILLER              PIC X.
               10  AP-DC-MM            PIC 9(2).
               10  FILLER              PIC X.
               10  AP-DC-DD            PIC 9(2).
      *>   Bacs Standard 18 - the bureau still wants fixed 100 byte
      *>   records; see docs\tech-debt\ap.md entry 8
       01  AP-BACS-WORK.
           05  AP-BX-SORT              PIC 9(6) VALUE 0.
           05  AP-BX-ACCOUNT           PIC 9(8) VALUE 0.
           05  AP-BX-TYPE              PIC 9(1) VALUE 0.
           05  AP-BX-PENCE             PIC 9(11) VALUE 0.
           05  AP-BX-NAME              PIC X(18) VALUE SPACES.
           05  AP-BX-REF               PIC X(18) VALUE SPACES.
           05  AP-BX-TRANCODE          PIC X(2) VALUE "99".
           05  AP-BX-JULIAN            PIC 9(5) VALUE 0.
           05  AP-BX-MOD-OK            PIC X VALUE "Y".
           05  AP-BX-WEIGHT-TAB.
               10  FILLER              PIC X(28) VALUE
                   "0708090A0B0C0D0E0F0G0H0I0J0K".
           05  AP-BX-TOTAL-PENCE       PIC 9(13) VALUE 0.
           05  AP-BX-ITEM-COUNT        PIC 9(7) VALUE 0.
