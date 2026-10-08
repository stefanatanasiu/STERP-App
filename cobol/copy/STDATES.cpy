      *>================================================================
      *> STDATES  - STERP shared date work areas and constants
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : COMMON SERVICES
      *> PURPOSE  : Standard split-date areas, day/month name tables and
      *>            the format codes understood by DATECALC.  COPY into
      *>            WORKING-STORAGE of any program that handles dates.
      *>            Compiles in fixed and free format (cols 8-72, *>).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1993-06-14 RJH  CR0188 SPLIT DATE AREAS FOR AGEING SUITE
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS, WINDOW REMOVED FROM COPY
      *> 2012-04-23 ANW  REWRITE FOR GNUCOBOL, DAY/MONTH NAME TABLES
      *> 2026-10-05 STERP CALENDAR CODES ENG/SCT/NIR/BACS (COM-026)
      *>================================================================
      *> REQ: COM-009, COM-026
       01  STD-DATE-WORK.
           05  STD-DATE-N              PIC 9(8) VALUE 0.
           05  STD-DATE-X REDEFINES STD-DATE-N.
               10  STD-YYYY            PIC 9(4).
               10  STD-YY-CC REDEFINES STD-YYYY.
                   15  STD-CC          PIC 9(2).
                   15  STD-YY          PIC 9(2).
               10  STD-MM              PIC 9(2).
               10  STD-DD              PIC 9(2).
           05  STD-INTEGER             PIC S9(9) VALUE 0.
           05  STD-DOW                 PIC S9(4) VALUE 0.
               88  STD-MONDAY                    VALUE 1.
               88  STD-TUESDAY                   VALUE 2.
               88  STD-WEDNESDAY                 VALUE 3.
               88  STD-THURSDAY                  VALUE 4.
               88  STD-FRIDAY                    VALUE 5.
               88  STD-SATURDAY                  VALUE 6.
               88  STD-SUNDAY                    VALUE 7.
               88  STD-WEEKEND                   VALUE 6 7.
               88  STD-WEEKDAY                   VALUE 1 THRU 5.
           05  STD-UK-DATE.
               10  STD-UK-DD           PIC 9(2) VALUE 0.
               10  FILLER              PIC X VALUE "/".
               10  STD-UK-MM           PIC 9(2) VALUE 0.
               10  FILLER              PIC X VALUE "/".
               10  STD-UK-YYYY         PIC 9(4) VALUE 0.
           05  STD-ISO-DATE.
               10  STD-ISO-YYYY        PIC 9(4) VALUE 0.
               10  FILLER              PIC X VALUE "-".
               10  STD-ISO-MM          PIC 9(2) VALUE 0.
               10  FILLER              PIC X VALUE "-".
               10  STD-ISO-DD          PIC 9(2) VALUE 0.
      *> format codes accepted by DATECALC DC-FORMAT-IN / DC-FORMAT-OUT
           05  STD-FMT-COMPACT         PIC X(8) VALUE "YYYYMMDD".
           05  STD-FMT-UK              PIC X(8) VALUE "DDMMYYYY".
           05  STD-FMT-ISO             PIC X(8) VALUE "ISO".
      *> business-day calendars maintained in core.Calendar (COM-026)
           05  STD-CAL-ENG             PIC X(10) VALUE "ENG".
           05  STD-CAL-SCT             PIC X(10) VALUE "SCT".
           05  STD-CAL-NIR             PIC X(10) VALUE "NIR".
           05  STD-CAL-BACS            PIC X(10) VALUE "BACS".
      *> day names, subscripted by STD-DOW (1 = Monday)
           05  STD-DAY-NAMES.
               10  FILLER              PIC X(9) VALUE "Monday".
               10  FILLER              PIC X(9) VALUE "Tuesday".
               10  FILLER              PIC X(9) VALUE "Wednesday".
               10  FILLER              PIC X(9) VALUE "Thursday".
               10  FILLER              PIC X(9) VALUE "Friday".
               10  FILLER              PIC X(9) VALUE "Saturday".
               10  FILLER              PIC X(9) VALUE "Sunday".
           05  STD-DAY-TAB REDEFINES STD-DAY-NAMES.
               10  STD-DAY-NAME        PIC X(9) OCCURS 7 TIMES.
      *> month names, subscripted by STD-MM (COM-024 month in words)
           05  STD-MONTH-NAMES.
               10  FILLER              PIC X(9) VALUE "January".
               10  FILLER              PIC X(9) VALUE "February".
               10  FILLER              PIC X(9) VALUE "March".
               10  FILLER              PIC X(9) VALUE "April".
               10  FILLER              PIC X(9) VALUE "May".
               10  FILLER              PIC X(9) VALUE "June".
               10  FILLER              PIC X(9) VALUE "July".
               10  FILLER              PIC X(9) VALUE "August".
               10  FILLER              PIC X(9) VALUE "September".
               10  FILLER              PIC X(9) VALUE "October".
               10  FILLER              PIC X(9) VALUE "November".
               10  FILLER              PIC X(9) VALUE "December".
           05  STD-MONTH-TAB REDEFINES STD-MONTH-NAMES.
               10  STD-MONTH-NAME      PIC X(9) OCCURS 12 TIMES.
      *> ordinary (non leap) days in month, subscripted by STD-MM
           05  STD-MONTH-LENGTHS.
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
           05  STD-MONTH-LEN-TAB REDEFINES STD-MONTH-LENGTHS.
               10  STD-MONTH-LEN       PIC 9(2) OCCURS 12 TIMES.
