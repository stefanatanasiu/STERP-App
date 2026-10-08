      *>================================================================
      *> XMDBATWK - STERP CROSS MODULE BATCH SUITE WORKING STORAGE
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : CROSS MODULE CONTROLS AND REPORTING (XMD)
      *> PURPOSE  : THE ARGUMENT, SPOOL, FETCH AND SCRATCH AREAS USED
      *>            BY EVERY PROGRAM OF THE 1993 CONTROL BATCH SUITE.
      *>            COPY IT AFTER SQLIOCA AND STRPTWS AND BEFORE THE
      *>            PROGRAM'S OWN DETAIL AREAS - XB-SPOOL-NAME IS THE
      *>            ASSIGN TARGET OF THE REPORT FILE AND MUST BE
      *>            DECLARED BEFORE IT IS REFERENCED.
      *>            Compiles in fixed and free format (cols 8-72, *>).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1993-04-19 DJM  CR0163 ORIGINAL - CONTROL ACCOUNT BATCH SUITE
      *> 1996-09-30 RJH  CR0514 OWNER AND ESCALATION FIELDS ADDED
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS THROUGHOUT
      *> 2026-10-06 STERP SQL SERVER PORT, 30 FETCH SLOTS OF 120 BYTES
      *>================================================================
      *> REQ: XMD-005, XMD-010, COM-018, COM-020
       01  XB-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  XB-FILE-STATUS              PIC X(2) VALUE "00".
      *>   the 1993 listing fetched 12 columns; the slot width has
      *>   been left at 120 since the SQL Server port (CR2601)
       01  BR-ROW.
           05  BR-COL OCCURS 30 TIMES  PIC X(120).
       01  XB-ARGS.
           05  XB-CO                   PIC X(10) VALUE SPACES.
           05  XB-SITE                 PIC X(10) VALUE SPACES.
           05  XB-USER                 PIC X(20) VALUE SPACES.
           05  XB-MODE                 PIC X(10) VALUE SPACES.
               88  XB-PREVIEW                    VALUE "PREVIEW".
               88  XB-COMMIT                     VALUE "COMMIT".
           05  XB-ASAT                 PIC 9(8) VALUE 0.
           05  XB-FROM                 PIC 9(8) VALUE 0.
           05  XB-TO                   PIC 9(8) VALUE 0.
           05  XB-FY                   PIC 9(4) VALUE 0.
           05  XB-PER                  PIC 9(2) VALUE 0.
           05  XB-PERIOD-KEY           PIC X(10) VALUE SPACES.
           05  XB-PERIOD-STATUS        PIC X(10) VALUE SPACES.
           05  XB-CODE                 PIC X(40) VALUE SPACES.
           05  XB-TYPE                 PIC X(20) VALUE SPACES.
           05  XB-OWNER                PIC X(20) VALUE SPACES.
           05  XB-FILE-ARG             PIC X(200) VALUE SPACES.
           05  XB-OUT-ARG              PIC X(200) VALUE SPACES.
       01  XB-RUN.
           05  XB-RC                   PIC S9(4) VALUE 0.
           05  XB-JOBID                PIC S9(9) COMP-5 VALUE 0.
           05  XB-ROWS-IN              PIC S9(9) VALUE 0.
           05  XB-ROWS-OUT             PIC S9(9) VALUE 0.
           05  XB-ACCEPTED             PIC S9(9) VALUE 0.
           05  XB-REJECTED             PIC S9(9) VALUE 0.
           05  XB-DUPLICATE            PIC S9(9) VALUE 0.
           05  XB-SKIPPED              PIC S9(9) VALUE 0.
           05  XB-RAISED               PIC S9(9) VALUE 0.
           05  XB-ESCALATED            PIC S9(9) VALUE 0.
           05  XB-POSTED               PIC S9(9) VALUE 0.
           05  XB-DIFFS                PIC S9(9) VALUE 0.
       01  XB-SCRATCH.
           05  XB-I                    PIC S9(4) VALUE 0.
           05  XB-J                    PIC S9(4) VALUE 0.
           05  XB-K                    PIC S9(4) VALUE 0.
           05  XB-NOW                  PIC X(21) VALUE SPACES.
           05  XB-STAMP                PIC X(14) VALUE SPACES.
           05  XB-NUM-DATE             PIC 9(8) VALUE 0.
           05  XB-DATE-ED              PIC 9(8) VALUE 0.
           05  XB-DATE-ED2             PIC 9(8) VALUE 0.
           05  XB-TEXT                 PIC X(200) VALUE SPACES.
           05  XB-TEXT2                PIC X(200) VALUE SPACES.
           05  XB-NUM-ED               PIC -(14)9.999999.
           05  XB-MON-ED               PIC -(14)9.99.
           05  XB-INT-ED               PIC -(16)9.
           05  XB-SEQ-ED               PIC 9(9) VALUE 0.
           05  XB-AMT                  PIC S9(13)V99 VALUE 0.
           05  XB-AMT2                 PIC S9(13)V99 VALUE 0.
           05  XB-QTY                  PIC S9(13)V9(6) VALUE 0.
           05  XB-PCT                  PIC S9(5)V9(4) VALUE 0.
           05  XB-TOLERANCE            PIC S9(9)V99 VALUE 0.01.
           05  XB-CORREL               PIC X(36) VALUE SPACES.
           05  XB-DONE                 PIC X VALUE "N".
               88  XB-FINISHED                   VALUE "Y".
       01  XB-UK.
           05  XB-UK-DD                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  XB-UK-MM                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  XB-UK-YYYY              PIC 9(4) VALUE 0.
       01  XB-UK2.
           05  XB-UK2-DD               PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  XB-UK2-MM               PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  XB-UK2-YYYY             PIC 9(4) VALUE 0.
