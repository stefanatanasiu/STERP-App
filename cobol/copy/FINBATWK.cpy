      *>================================================================
      *> FINBATWK - STERP FINANCE BATCH SUITE COMMON WORKING STORAGE
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : GENERAL LEDGER (FIN)
      *> PURPOSE  : THE ARGUMENT, SPOOL, FETCH AND SCRATCH AREAS THAT
      *>            EVERY FIN BATCH PROGRAM NEEDS.  COPY IT AFTER
      *>            SQLIOCA AND STRPTWS, BEFORE THE PROGRAM'S OWN
      *>            DETAIL AREAS.  WS-SPOOL-NAME IS THE ASSIGN TARGET
      *>            OF THE REPORT FILE, SO THE COPY MUST COME BEFORE
      *>            THE FIRST REFERENCE IN A REPORT PROGRAM.
      *>            Compiles in fixed and free format (cols 8-72, *>).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-03-07 PGW  CR0255 ORIGINAL - THE 1994 LEDGER BATCH SUITE
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS THROUGHOUT
      *> 2026-10-06 STERP SQL SERVER PORT, FETCH SLOTS WIDENED TO 120
      *>================================================================
      *> REQ: FIN-023
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
      *>   the 1994 listing fetched 12 columns; the slot width has
      *>   been left at 120 since the SQL Server port (CR2601)
       01  BR-ROW.
           05  BR-COL OCCURS 30 TIMES  PIC X(120).
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-MODE                 PIC X(8) VALUE SPACES.
           05  WS-ASAT                 PIC 9(8) VALUE 0.
           05  WS-FROM                 PIC 9(8) VALUE 0.
           05  WS-TO                   PIC 9(8) VALUE 0.
           05  WS-FY                   PIC 9(4) VALUE 0.
           05  WS-PER                  PIC 9(2) VALUE 0.
           05  WS-PER-FROM             PIC 9(2) VALUE 0.
           05  WS-PER-TO               PIC 9(2) VALUE 0.
           05  WS-PERIOD-KEY           PIC X(10) VALUE SPACES.
           05  WS-CODE                 PIC X(20) VALUE SPACES.
           05  WS-ACCOUNT              PIC X(12) VALUE SPACES.
           05  WS-CURRENCY             PIC X(3) VALUE SPACES.
           05  WS-OUT-ARG              PIC X(200) VALUE SPACES.
       01  WS-RUN.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-JOBID                PIC S9(9) COMP-5 VALUE 0.
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-SKIPPED              PIC S9(9) VALUE 0.
           05  WS-POSTED               PIC S9(9) VALUE 0.
           05  WS-BLOCKED              PIC S9(9) VALUE 0.
       01  WS-SCRATCH.
           05  FB-I                    PIC S9(4) VALUE 0.
           05  FB-J                    PIC S9(4) VALUE 0.
           05  FB-K                    PIC S9(4) VALUE 0.
           05  FB-NOW                  PIC X(21) VALUE SPACES.
           05  FB-NUM-DATE             PIC 9(8) VALUE 0.
           05  FB-DATE-ED              PIC 9(8) VALUE 0.
           05  FB-TEXT                 PIC X(200) VALUE SPACES.
           05  FB-NUM-ED               PIC -(14)9.99.
           05  FB-INT-ED               PIC -(16)9.
           05  FB-AMT                  PIC S9(13)V99 VALUE 0.
           05  FB-AMT2                 PIC S9(13)V99 VALUE 0.
           05  FB-CORREL               PIC X(36) VALUE SPACES.
           05  FB-SEQ-ED               PIC 9(6) VALUE 0.
       01  WS-UK.
           05  WS-UK-DD                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-UK-MM                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-UK-YYYY              PIC 9(4) VALUE 0.
