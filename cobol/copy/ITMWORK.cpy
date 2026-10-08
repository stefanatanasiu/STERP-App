      *>================================================================
      *> ITMWORK - shared working storage for the stock control suite.
      *> SYSTEM  : STERP - Stefan's ERP
      *> MODULE  : ITEM AND PART MAINTENANCE
      *>
      *> The 1996 stock control team standardised on one wide fetch
      *> buffer rather than a record layout per SELECT: every column is
      *> fetched into a 100 character slot and converted on the way
      *> out.  It costs memory and it hides the real column types, but
      *> it means a new enquiry needs no new copybook.  See
      *> docs\tech-debt\itm.md.
      *>
      *> Usage
      *>     COPY ITMWORK.
      *>     MOVE 6 TO SQLIO-COL-CNT
      *>     PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
      *>         MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
      *>     END-PERFORM
      *>     ...  CALL "SQLIO" USING SQLIO-CA ITM-ROW
      *>     COMPUTE WS-QTY = FUNCTION NUMVAL(ITM-COL(4))
      *>
      *> ITM-DATE-TEXT holds an ODBC date (YYYY-MM-DD); WS-DATE-CONV
      *> turns it into the YYYYMMDD number the COMMAREAs carry.
      *> Compiles in fixed and free format (cols 8-72, *> comments).
      *>----------------------------------------------------------------
      *> 1996-02-11 RJH  CR0412 ORIGINAL
      *> 2026-10-05 STERP WIDENED TO 24 COLUMNS
      *>================================================================
       01  ITM-ROW.
           05  ITM-COL OCCURS 24 TIMES PIC X(100).
       01  ITM-SCRATCH.
           05  WS-IX                   PIC S9(4) VALUE 0.
           05  WS-JX                   PIC S9(4) VALUE 0.
           05  WS-KX                   PIC S9(4) VALUE 0.
           05  WS-ROWS                 PIC S9(9) VALUE 0.
           05  WS-QTY                  PIC S9(13)V9(6) VALUE 0.
           05  WS-QTY2                 PIC S9(13)V9(6) VALUE 0.
           05  WS-AMT                  PIC S9(13)V99 VALUE 0.
           05  WS-FACTOR               PIC S9(9)V9(9) VALUE 0.
           05  WS-TEXT                 PIC X(200) VALUE SPACES.
           05  WS-TEXT2                PIC X(200) VALUE SPACES.
           05  WS-CODE                 PIC X(40) VALUE SPACES.
           05  WS-FLAG                 PIC X VALUE "N".
           05  WS-SQL-FAILED           PIC X VALUE "N".
               88  WS-SQL-BAD                    VALUE "Y".
           05  WS-DONE                 PIC X VALUE "N".
               88  WS-AT-END                     VALUE "Y".
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
           05  WS-QTY-ED               PIC Z(9)9.9(6).
