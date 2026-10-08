      *>================================================================
      *> ARWORK  - WORKING STORAGE SHARED BY THE SALES LEDGER PROGRAMS
      *>           COPY ARWORK. AFTER COPY SQLIOCA.
      *>           Holds the date conversion fields, numeric edit
      *>           masks and the small switches every AR program uses.
      *>           Compiles in fixed and free format (cols 8-72, *>).
      *>
      *>           AR-CTL holds the standard header values while the
      *>           program runs.  An AR program usually has several
      *>           COMMAREAs in scope (its own plus the common service
      *>           ones), which makes a bare CA-RETURN-CODE ambiguous,
      *>           so the body works on AR-RC / AR-REASON / AR-MSG and
      *>           one section copies them back to DFHCOMMAREA at the
      *>           end.  That is also how the original MVS programs
      *>           handled their CICS copybooks.
      *> 1991-04-16 ORIGINAL SALES LEDGER PORT (MVS)
      *> 1999-11-30 KMP Y2K - 4 DIGIT YEARS THROUGHOUT
      *> 2026-10-05 STERP SQL SERVER COLUMN WIDTHS
      *>================================================================
       01  AR-CTL.
           05  AR-RC                   PIC 9(2) VALUE 0.
               88  AR-RC-OK                      VALUE 00.
               88  AR-RC-WARN                    VALUE 04.
               88  AR-RC-HOLD                    VALUE 06.
               88  AR-RC-ERROR                   VALUE 08.
               88  AR-RC-SEVERE                  VALUE 12.
               88  AR-RC-NOTFOUND                VALUE 16.
               88  AR-RC-BAD                     VALUE 08 THRU 16.
      *>       a hold (06) is as final as an error for anything that
      *>       would otherwise post - see RCPTALOC D-22 write offs
               88  AR-RC-STOP                    VALUE 06 THRU 16.
           05  AR-REASON               PIC X(8) VALUE SPACES.
           05  AR-MSG                  PIC X(200) VALUE SPACES.
           05  AR-MSG-CNT              PIC 9(2) VALUE 0.
           05  AR-FUNC                 PIC X(8) VALUE SPACES.
           05  AR-CO                   PIC X(10) VALUE SPACES.
           05  AR-SITE                 PIC X(10) VALUE SPACES.
           05  AR-USERID               PIC X(20) VALUE SPACES.
           05  AR-BUSDATE              PIC 9(8) VALUE 0.
           05  AR-CORREL               PIC X(36) VALUE SPACES.
           05  AR-REQ-REF              PIC X(10) VALUE SPACES.
       01  AR-WORK.
           05  AR-ERROR                PIC X VALUE "N".
               88  AR-INPUT-BAD                  VALUE "Y".
           05  AR-DONE                 PIC X VALUE "N".
               88  AR-FINISHED                   VALUE "Y".
           05  AR-IN-TRANSACTION       PIC X VALUE "N".
           05  AR-I                    PIC S9(4) VALUE 0.
           05  AR-J                    PIC S9(4) VALUE 0.
           05  AR-K                    PIC S9(4) VALUE 0.
           05  AR-N                    PIC S9(9) VALUE 0.
           05  AR-ROWS                 PIC S9(9) VALUE 0.
           05  AR-NOW                  PIC X(21) VALUE SPACES.
           05  AR-TODAY                PIC 9(8) VALUE 0.
           05  AR-AMT1                 PIC S9(13)V99 VALUE 0.
           05  AR-AMT2                 PIC S9(13)V99 VALUE 0.
           05  AR-AMT3                 PIC S9(13)V99 VALUE 0.
           05  AR-RATE1                PIC S9(5)V9(8) VALUE 0.
           05  AR-QTY1                 PIC S9(9)V9(6) VALUE 0.
           05  AR-PRICE1               PIC S9(9)V9(6) VALUE 0.
           05  AR-PCT1                 PIC S9(5)V9(6) VALUE 0.
           05  AR-DAYS                 PIC S9(7) VALUE 0.
           05  AR-TEXT                 PIC X(200) VALUE SPACES.
           05  AR-TEXT2                PIC X(200) VALUE SPACES.
           05  AR-SCRATCH              PIC X(256) VALUE SPACES.
      *>   yyyy-mm-dd from SQL Server <-> yyyymmdd used in COBOL
       01  AR-ISO-DATE.
           05  AR-ISO-Y                PIC X(4).
           05  FILLER                  PIC X.
           05  AR-ISO-M                PIC X(2).
           05  FILLER                  PIC X.
           05  AR-ISO-D                PIC X(2).
       01  AR-COMPACT-DATE.
           05  AR-CMP-Y                PIC X(4).
           05  AR-CMP-M                PIC X(2).
           05  AR-CMP-D                PIC X(2).
       01  AR-NUM-DATE                 PIC 9(8) VALUE 0.
      *>   edit masks for building SQL parameters and messages
       01  AR-EDITS.
           05  AR-MONEY-ED             PIC -(13)9.99.
           05  AR-QTY-ED               PIC -(10)9.9(6).
           05  AR-RATE-ED              PIC -(5)9.9(8).
           05  AR-INT-ED               PIC -(9)9.
           05  AR-DATE-ED              PIC 9(8).
           05  AR-MONEY-UK             PIC Z,ZZZ,ZZZ,ZZ9.99-.
      *>   row buffer used by the single row lookups
       01  AR-ROW-BUF.
           05  AR-C1                   PIC X(60).
           05  AR-C2                   PIC X(60).
           05  AR-C3                   PIC X(60).
           05  AR-C4                   PIC X(60).
           05  AR-C5                   PIC X(60).
           05  AR-C6                   PIC X(60).
           05  AR-C7                   PIC X(60).
           05  AR-C8                   PIC X(60).
           05  AR-C9                   PIC X(60).
           05  AR-C10                  PIC X(60).
           05  AR-C11                  PIC X(60).
           05  AR-C12                  PIC X(60).
           05  AR-C13                  PIC X(60).
           05  AR-C14                  PIC X(60).
           05  AR-C15                  PIC X(60).
           05  AR-C16                  PIC X(60).
