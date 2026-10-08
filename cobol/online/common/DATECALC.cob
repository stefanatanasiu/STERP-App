      *>================================================================
      *> PROGRAM-ID : DATECALC
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COMMON SERVICES
      *> PURPOSE    : DATE VALIDATION, ARITHMETIC, FORMATTING AND UK
      *>              BUSINESS DAY CALCULATION FOR THE WHOLE SYSTEM.
      *>              COM-009 - BUSINESS EVENT DATES, POSTING DATES AND
      *>              TAX DATES ARE DISTINCT; THIS PROGRAM ONLY SAYS
      *>              WHETHER A DATE IS VALID AND WHAT IT BECOMES AFTER
      *>              A RULE.  PERIOD POLICY LIVES IN PERIODCK.
      *>              COM-026 - BUSINESS DAY CALENDARS ENG (ENGLAND AND
      *>              WALES, THE DEFAULT), SCT, NIR AND THE BACS
      *>              PROCESSING CALENDAR.  HOLIDAYS COME FROM
      *>              core.CalendarDay / core.Holiday AND ARE CACHED IN
      *>              STORAGE PER CALENDAR AND YEAR.  WHEN THE DATABASE
      *>              IS NOT AVAILABLE THE BUILT-IN TABLE IN COPYBOOK
      *>              CALBUILT IS USED AND DC-CAL-SOURCE SAYS BUILTIN.
      *> CALLED BY  : EVERY MODULE, TERMCALC, THE BATCH SUITE AND THE
      *>              .NET CLIENT THROUGH THE COMMAREA BRIDGE
      *> INTERFACE  : CALL "DATECALC" USING WS-DATECALC-CA
      *>              COPYBOOK DATECALCCA
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1993-06-14 RJH  CR0188 DATE ROUTINE FOR THE AGEING SUITE
      *> 1996-01-08 DJM  CR0501 BANK HOLIDAY TABLE, BACS PROCESSING DAYS
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS.  THE OLD 2 DIGIT
      *>                 WINDOW (IF YY < 50 ADD 2000) SURVIVES IN THE
      *>                 PARSE FUNCTION FOR OLD INTERFACE FILES ONLY
      *> 2004-02-17 KMP  CR1890 IN STORAGE CACHE PER CALENDAR AND YEAR
      *> 2012-04-23 ANW  REWRITE FOR GNUCOBOL - INTRINSICS, EVALUATE
      *> 2026-10-05 STERP CALENDARS READ FROM core.CalendarDay,
      *>                 NIR CALENDAR ADDED, ADJUST DIRECTION (AC-112)
      *>================================================================
      *> REQ: COM-009, COM-026   AC: AC-112
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATECALC.
       AUTHOR. R J HARGREAVES.
       DATE-WRITTEN. 14/06/1993.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       REPOSITORY.
           FUNCTION DAYS-IN-MONTH
           FUNCTION END-OF-MONTH
           FUNCTION WEEKDAY-OF.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STDATES.
       COPY STFNARGS.
       COPY CALBUILT.
      *>---------------------------------------------------------------
      *> THE CALENDAR CACHE DELIBERATELY SURVIVES BETWEEN CALLS - IT IS
      *> REFERENCE DATA, NOT PER CALL STATE.  EVERYTHING ELSE IS RESET
      *> IN 0100-INITIALISE.  CACHECLR EMPTIES IT.
      *>---------------------------------------------------------------
       01  WS-CACHE.
           05  WS-CACHE-SLOTS          PIC S9(4) VALUE 12.
           05  WS-CACHE-TICK           PIC S9(9) COMP-5 VALUE 0.
           05  WS-CACHE-HITS           PIC S9(9) COMP-5 VALUE 0.
           05  WS-CACHE-LOADS          PIC S9(9) COMP-5 VALUE 0.
           05  WS-SLOT OCCURS 12 TIMES.
               10  WS-SL-CAL           PIC X(10).
               10  WS-SL-YEAR          PIC 9(4).
               10  WS-SL-USED          PIC S9(9) COMP-5.
               10  WS-SL-SOURCE        PIC X(8).
               10  WS-SL-ROWS          PIC S9(9) COMP-5.
               10  WS-SL-DAY           PIC X OCCURS 366 TIMES.
               10  WS-SL-NAME OCCURS 20 TIMES.
                   15  WS-SL-N-DAY     PIC S9(4).
                   15  WS-SL-N-TEXT    PIC X(28).
               10  WS-SL-NAME-CNT      PIC S9(4).
       01  WS-PROBE.
           05  WS-PROBE-DONE           PIC X VALUE "N".
           05  WS-CAL-TABLE            PIC X(2) VALUE "NO".
               88  WS-USE-CALENDARDAY            VALUE "CD".
               88  WS-USE-HOLIDAY                VALUE "HD".
               88  WS-NO-CAL-TABLE               VALUE "NO".
       01  WS-WORK.
           05  WS-I                    PIC S9(9) COMP-5 VALUE 0.
           05  WS-J                    PIC S9(9) COMP-5 VALUE 0.
           05  WS-SLOT-NO              PIC S9(9) COMP-5 VALUE 0.
           05  WS-OLDEST               PIC S9(9) COMP-5 VALUE 0.
           05  WS-DOY                  PIC S9(9) COMP-5 VALUE 0.
           05  WS-JAN1                 PIC 9(8) VALUE 0.
           05  WS-INT                  PIC S9(9) VALUE 0.
           05  WS-INT2                 PIC S9(9) VALUE 0.
           05  WS-STEP                 PIC S9(4) VALUE 1.
           05  WS-COUNTED              PIC S9(9) COMP-5 VALUE 0.
           05  WS-WANTED               PIC S9(9) COMP-5 VALUE 0.
           05  WS-GUARD                PIC S9(9) COMP-5 VALUE 0.
           05  WS-CAL                  PIC X(10) VALUE SPACES.
           05  WS-LOAD-CAL             PIC X(10) VALUE SPACES.
           05  WS-YEAR                 PIC 9(4) VALUE 0.
           05  WS-MONTHS               PIC S9(9) VALUE 0.
           05  WS-TARGET-M             PIC S9(9) VALUE 0.
           05  WS-TARGET-Y             PIC S9(9) VALUE 0.
           05  WS-LAST-DAY             PIC S9(4) VALUE 0.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-WORK-DATE            PIC 9(8) VALUE 0.
           05  WS-SAVE-DATE            PIC 9(8) VALUE 0.
           05  WS-FORCE-BUILTIN        PIC X VALUE "N".
           05  WS-NUM-ED               PIC -(9)9.
       01  WS-PARSE.
           05  WS-TXT                  PIC X(10) VALUE SPACES.
           05  WS-D2                   PIC 9(2) VALUE 0.
           05  WS-M2                   PIC 9(2) VALUE 0.
           05  WS-Y4                   PIC 9(4) VALUE 0.
           05  WS-Y2                   PIC 9(2) VALUE 0.
           05  WS-DIGITS               PIC X(8) VALUE SPACES.
           05  WS-DIG-LEN              PIC S9(4) VALUE 0.
       01  WS-SQL-ROW.
           05  WS-SQL-DATE             PIC X(8).
           05  WS-SQL-NAME             PIC X(28).
       01  WS-SQL-PROBE                PIC X(2).
       LINKAGE SECTION.
       COPY DATECALCCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           EVALUATE CA-FUNCTION
               WHEN "CACHECLR" PERFORM 7900-CLEAR-CACHE
               WHEN "VALIDATE" PERFORM 1000-VALIDATE
               WHEN "ADDDAYS"  PERFORM 1100-ADD-DAYS
               WHEN "ADDMONTH" PERFORM 1200-ADD-MONTHS
               WHEN "EOMONTH"  PERFORM 1300-END-OF-MONTH
               WHEN "DOW"      PERFORM 1400-DAY-OF-WEEK
               WHEN "DAYSBTWN" PERFORM 1500-DAYS-BETWEEN
               WHEN "FORMAT"   PERFORM 1600-FORMAT
               WHEN "PARSE"    PERFORM 1700-PARSE
               WHEN "ISBUSDAY" PERFORM 2000-IS-BUSINESS-DAY
               WHEN "ADDBUS"   PERFORM 2100-ADD-BUSINESS-DAYS
               WHEN "ADJUST"   PERFORM 2200-ADJUST
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE WS-WORK WS-PARSE
           MOVE "N" TO WS-ERROR
           MOVE "DATECALC" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "COM-026" TO CA-REQ-REF
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO DC-DAY-NAME DC-MONTH-NAME DC-HOLIDAY-NAME
           MOVE SPACES TO DC-TEXT-OUT
           MOVE "N" TO DC-IS-BUSINESS DC-IS-HOLIDAY DC-IS-WEEKEND
           MOVE "N" TO DC-ADJUSTED
           MOVE 0 TO DC-RESULT-NUM
           MOVE 0 TO DC-RESULT-DATE
           MOVE 0 TO DC-CAL-ROWS
      *>   DC-CAL-SOURCE is normally an output.  A caller that sets it
      *>   to BUILTIN on entry asks for the built-in holiday table and
      *>   no database read (offline engine, and repeatable tests).
           MOVE "N" TO WS-FORCE-BUILTIN
           IF DC-CAL-SOURCE = "BUILTIN"
               MOVE "Y" TO WS-FORCE-BUILTIN
           END-IF
           MOVE SPACES TO DC-CAL-SOURCE
           MOVE WS-CACHE-HITS TO DC-CACHE-HITS
           MOVE WS-CACHE-LOADS TO DC-CACHE-LOADS
      *>   England and Wales is the default calendar (COM-026)
           IF DC-CALENDAR = SPACES OR DC-CALENDAR = LOW-VALUES
               MOVE "ENG" TO DC-CALENDAR
           END-IF
           MOVE FUNCTION UPPER-CASE(DC-CALENDAR) TO WS-CAL
           MOVE WS-CAL TO DC-CALENDAR.
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> SIMPLE DATE SERVICES
      *>----------------------------------------------------------------
       1000-VALIDATE SECTION.
       1000-START.
           PERFORM 8000-CHECK-DATE-1
           IF NOT WS-INPUT-BAD
               MOVE DC-DATE-1 TO DC-RESULT-DATE
               MOVE 0 TO DC-RESULT-NUM
               MOVE "Date is valid" TO CA-MESSAGE
           END-IF.
       1000-EXIT.
           EXIT.

       1100-ADD-DAYS SECTION.
       1100-START.
           PERFORM 8000-CHECK-DATE-1
           IF WS-INPUT-BAD
               GO TO 1100-EXIT
           END-IF
           COMPUTE WS-INT = FUNCTION INTEGER-OF-DATE(DC-DATE-1)
                            + DC-NUMBER
           IF WS-INT < 1 OR WS-INT > 3067671
               MOVE "DAT0005" TO CA-REASON-CODE
               MOVE "Resulting date is outside the supported range"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1100-EXIT
           END-IF
           COMPUTE DC-RESULT-DATE = FUNCTION DATE-OF-INTEGER(WS-INT)
           MOVE DC-RESULT-DATE TO DC-DATE-2
           MOVE "Calendar days added" TO CA-MESSAGE.
       1100-EXIT.
           EXIT.

      *>   A day that does not exist in the target month becomes that
      *>   month's final day - spec 01 section 9.  31 Jan plus one month
      *>   is 28 or 29 February.
       1200-ADD-MONTHS SECTION.
       1200-START.
           PERFORM 8000-CHECK-DATE-1
           IF WS-INPUT-BAD
               GO TO 1200-EXIT
           END-IF
           MOVE DC-DATE-1 TO STD-DATE-N
           COMPUTE WS-MONTHS = (STD-YYYY * 12) + STD-MM - 1 + DC-NUMBER
           COMPUTE WS-TARGET-Y = WS-MONTHS / 12
           COMPUTE WS-TARGET-M = WS-MONTHS - (WS-TARGET-Y * 12) + 1
           IF WS-TARGET-Y < 1601 OR WS-TARGET-Y > 9999
               MOVE "DAT0005" TO CA-REASON-CODE
               MOVE "Resulting date is outside the supported range"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1200-EXIT
           END-IF
           MOVE WS-TARGET-Y TO FNA-YEAR
           MOVE WS-TARGET-M TO FNA-MONTH
           COMPUTE WS-LAST-DAY =
               FUNCTION DAYS-IN-MONTH(FNA-YEAR, FNA-MONTH)
           MOVE WS-TARGET-Y TO STD-YYYY
           MOVE WS-TARGET-M TO STD-MM
           IF STD-DD > WS-LAST-DAY
               MOVE WS-LAST-DAY TO STD-DD
           END-IF
           MOVE STD-DATE-N TO DC-RESULT-DATE
           MOVE DC-RESULT-DATE TO DC-DATE-2
           MOVE "Calendar months added, day clamped to month end"
               TO CA-MESSAGE.
       1200-EXIT.
           EXIT.

       1300-END-OF-MONTH SECTION.
       1300-START.
           PERFORM 8000-CHECK-DATE-1
           IF WS-INPUT-BAD
               GO TO 1300-EXIT
           END-IF
           IF DC-NUMBER NOT = 0
               PERFORM 1200-ADD-MONTHS
               IF WS-INPUT-BAD
                   GO TO 1300-EXIT
               END-IF
               MOVE DC-RESULT-DATE TO WS-WORK-DATE
           ELSE
               MOVE DC-DATE-1 TO WS-WORK-DATE
           END-IF
           MOVE WS-WORK-DATE TO FNA-DATE
           MOVE FUNCTION END-OF-MONTH(FNA-DATE) TO DC-RESULT-DATE
           MOVE DC-RESULT-DATE TO DC-DATE-2
           MOVE "Last calendar day of the month" TO CA-MESSAGE.
       1300-EXIT.
           EXIT.

       1400-DAY-OF-WEEK SECTION.
       1400-START.
           PERFORM 8000-CHECK-DATE-1
           IF WS-INPUT-BAD
               GO TO 1400-EXIT
           END-IF
           MOVE DC-DATE-1 TO FNA-DATE
           COMPUTE DC-RESULT-NUM = FUNCTION WEEKDAY-OF(FNA-DATE)
           MOVE DC-RESULT-NUM TO STD-DOW
           MOVE STD-DAY-NAME(STD-DOW) TO DC-DAY-NAME
           MOVE DC-DATE-1 TO STD-DATE-N
           MOVE STD-MONTH-NAME(STD-MM) TO DC-MONTH-NAME
           IF STD-WEEKEND
               MOVE "Y" TO DC-IS-WEEKEND
           END-IF
           MOVE DC-DATE-1 TO DC-RESULT-DATE
           MOVE SPACES TO CA-MESSAGE
           STRING FUNCTION TRIM(DC-DAY-NAME)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       1400-EXIT.
           EXIT.

       1500-DAYS-BETWEEN SECTION.
       1500-START.
           PERFORM 8000-CHECK-DATE-1
           IF WS-INPUT-BAD
               GO TO 1500-EXIT
           END-IF
           IF FUNCTION TEST-DATE-YYYYMMDD(DC-DATE-2) NOT = 0
               MOVE "DAT0002" TO CA-REASON-CODE
               MOVE "Second date is not a valid calendar date"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1500-EXIT
           END-IF
           COMPUTE WS-INT = FUNCTION INTEGER-OF-DATE(DC-DATE-1)
           COMPUTE WS-INT2 = FUNCTION INTEGER-OF-DATE(DC-DATE-2)
           COMPUTE DC-RESULT-NUM = WS-INT2 - WS-INT
           MOVE SPACES TO CA-MESSAGE
           MOVE DC-RESULT-NUM TO WS-NUM-ED
           STRING FUNCTION TRIM(WS-NUM-ED) " day(s) between the dates"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       1500-EXIT.
           EXIT.

      *>   COM-024 - dates are always explicit: dd/mm/yyyy for users,
      *>   yyyy-mm-dd for interfaces, yyyymmdd for storage and SQL
       1600-FORMAT SECTION.
       1600-START.
           PERFORM 8000-CHECK-DATE-1
           IF WS-INPUT-BAD
               GO TO 1600-EXIT
           END-IF
           MOVE DC-DATE-1 TO STD-DATE-N
           MOVE DC-DATE-1 TO DC-RESULT-DATE
           MOVE STD-MONTH-NAME(STD-MM) TO DC-MONTH-NAME
           EVALUATE DC-FORMAT-OUT
               WHEN "DDMMYYYY"
                   MOVE STD-DD TO STD-UK-DD
                   MOVE STD-MM TO STD-UK-MM
                   MOVE STD-YYYY TO STD-UK-YYYY
                   MOVE STD-UK-DATE TO DC-TEXT-OUT
               WHEN "ISO"
                   MOVE STD-YYYY TO STD-ISO-YYYY
                   MOVE STD-MM TO STD-ISO-MM
                   MOVE STD-DD TO STD-ISO-DD
                   MOVE STD-ISO-DATE TO DC-TEXT-OUT
               WHEN "YYYYMMDD"
                   MOVE DC-DATE-1 TO DC-TEXT-OUT
               WHEN SPACES
                   MOVE DC-DATE-1 TO DC-TEXT-OUT
               WHEN OTHER
                   MOVE "DAT0003" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Unknown date format "
                          FUNCTION TRIM(DC-FORMAT-OUT)
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   PERFORM 9000-REJECT
                   GO TO 1600-EXIT
           END-EVALUATE
           MOVE "Date formatted" TO CA-MESSAGE.
       1600-EXIT.
           EXIT.

      *>   PARSE accepts dd/mm/yyyy, yyyy-mm-dd and yyyymmdd.  Any
      *>   punctuation is stripped first, which also lets dd.mm.yyyy and
      *>   dd-mm-yyyy through - old interface files used both.
       1700-PARSE SECTION.
       1700-START.
           MOVE DC-TEXT-IN TO WS-TXT
           MOVE SPACES TO WS-DIGITS
           MOVE 0 TO WS-DIG-LEN
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
               IF WS-TXT(WS-I:1) NUMERIC
                   IF WS-DIG-LEN < 8
                       ADD 1 TO WS-DIG-LEN
                       MOVE WS-TXT(WS-I:1) TO WS-DIGITS(WS-DIG-LEN:1)
                   END-IF
               END-IF
           END-PERFORM
           EVALUATE TRUE
               WHEN WS-DIG-LEN = 8
                   PERFORM 1710-PARSE-EIGHT
               WHEN WS-DIG-LEN = 6
                   PERFORM 1720-PARSE-SIX
               WHEN OTHER
                   MOVE "DAT0004" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Cannot read a date from ["
                          FUNCTION TRIM(DC-TEXT-IN) "]"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   PERFORM 9000-REJECT
           END-EVALUATE
           IF WS-INPUT-BAD
               GO TO 1700-EXIT
           END-IF
           IF FUNCTION TEST-DATE-YYYYMMDD(WS-WORK-DATE) NOT = 0
               MOVE "DAT0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "["  FUNCTION TRIM(DC-TEXT-IN)
                      "] is not a valid calendar date"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1700-EXIT
           END-IF
           MOVE WS-WORK-DATE TO DC-RESULT-DATE
           MOVE WS-WORK-DATE TO DC-DATE-1
           MOVE "Date read" TO CA-MESSAGE.
       1700-EXIT.
           EXIT.

       1710-PARSE-EIGHT SECTION.
       1710-START.
           IF DC-FORMAT-IN = "DDMMYYYY"
               MOVE WS-DIGITS(1:2) TO WS-D2
               MOVE WS-DIGITS(3:2) TO WS-M2
               MOVE WS-DIGITS(5:4) TO WS-Y4
           ELSE
               IF DC-FORMAT-IN = "YYYYMMDD" OR DC-FORMAT-IN = "ISO"
                  OR DC-FORMAT-IN = SPACES
                   MOVE WS-DIGITS(1:4) TO WS-Y4
                   MOVE WS-DIGITS(5:2) TO WS-M2
                   MOVE WS-DIGITS(7:2) TO WS-D2
               ELSE
                   MOVE "DAT0003" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Unknown date format "
                          FUNCTION TRIM(DC-FORMAT-IN)
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   PERFORM 9000-REJECT
                   GO TO 1710-EXIT
               END-IF
           END-IF
           MOVE WS-Y4 TO STD-YYYY
           MOVE WS-M2 TO STD-MM
           MOVE WS-D2 TO STD-DD
           MOVE STD-DATE-N TO WS-WORK-DATE.
       1710-EXIT.
           EXIT.

      *>   LEGACY: six digit dates still arrive on a handful of old
      *>   interface files.  The 1999 Y2K window is kept for those and
      *>   is NOT used anywhere else (see docs\tech-debt\common.md).
       1720-PARSE-SIX SECTION.
       1720-START.
           MOVE WS-DIGITS(1:2) TO WS-D2
           MOVE WS-DIGITS(3:2) TO WS-M2
           MOVE WS-DIGITS(5:2) TO WS-Y2
           IF WS-Y2 < 50
               COMPUTE WS-Y4 = 2000 + WS-Y2
           ELSE
               COMPUTE WS-Y4 = 1900 + WS-Y2
           END-IF
           MOVE WS-Y4 TO STD-YYYY
           MOVE WS-M2 TO STD-MM
           MOVE WS-D2 TO STD-DD
           MOVE STD-DATE-N TO WS-WORK-DATE
           MOVE 4 TO CA-RETURN-CODE
           MOVE "DAT0007" TO CA-REASON-CODE.
       1720-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> BUSINESS DAY SERVICES (COM-026)
      *>----------------------------------------------------------------
       2000-IS-BUSINESS-DAY SECTION.
       2000-START.
           PERFORM 8000-CHECK-DATE-1
           IF WS-INPUT-BAD
               GO TO 2000-EXIT
           END-IF
           MOVE DC-DATE-1 TO WS-WORK-DATE
           PERFORM 8100-TEST-BUSINESS-DAY
           IF WS-INPUT-BAD
               GO TO 2000-EXIT
           END-IF
           MOVE DC-DATE-1 TO DC-RESULT-DATE
           IF DC-IS-BUSINESS = "Y"
               MOVE 1 TO DC-RESULT-NUM
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(DC-DAY-NAME)
                      " is a business day on calendar "
                      FUNCTION TRIM(DC-CALENDAR)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               MOVE 0 TO DC-RESULT-NUM
               PERFORM 8300-NOT-BUSINESS-MESSAGE
           END-IF.
       2000-EXIT.
           EXIT.

      *>   Add (or subtract) business days.  The starting date is never
      *>   counted; the result is always a business day.
       2100-ADD-BUSINESS-DAYS SECTION.
       2100-START.
           PERFORM 8000-CHECK-DATE-1
           IF WS-INPUT-BAD
               GO TO 2100-EXIT
           END-IF
           MOVE DC-NUMBER TO WS-WANTED
           IF WS-WANTED < 0
               MOVE -1 TO WS-STEP
               COMPUTE WS-WANTED = WS-WANTED * -1
           ELSE
               MOVE 1 TO WS-STEP
           END-IF
           MOVE DC-DATE-1 TO WS-WORK-DATE
           MOVE 0 TO WS-COUNTED
           MOVE 0 TO WS-GUARD
           PERFORM UNTIL WS-COUNTED >= WS-WANTED
                      OR WS-GUARD > 3650
                      OR WS-INPUT-BAD
               ADD 1 TO WS-GUARD
               COMPUTE WS-INT = FUNCTION INTEGER-OF-DATE(WS-WORK-DATE)
                                + WS-STEP
               COMPUTE WS-WORK-DATE = FUNCTION DATE-OF-INTEGER(WS-INT)
               PERFORM 8100-TEST-BUSINESS-DAY
               IF DC-IS-BUSINESS = "Y"
                   ADD 1 TO WS-COUNTED
               END-IF
           END-PERFORM
           IF WS-INPUT-BAD
               GO TO 2100-EXIT
           END-IF
           IF WS-GUARD > 3650
               MOVE "DAT0006" TO CA-REASON-CODE
               MOVE "Business day search exceeded ten years"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 2100-EXIT
           END-IF
      *>   zero business days still lands on a business day
           IF WS-WANTED = 0
               MOVE DC-DATE-1 TO WS-WORK-DATE
               PERFORM 8100-TEST-BUSINESS-DAY
               IF DC-IS-BUSINESS NOT = "Y"
                   MOVE "NEXT" TO DC-DIRECTION
                   PERFORM 8200-MOVE-TO-BUSINESS
               END-IF
           END-IF
           MOVE WS-WORK-DATE TO DC-RESULT-DATE
           MOVE WS-WORK-DATE TO DC-DATE-2
           MOVE SPACES TO CA-MESSAGE
           MOVE DC-NUMBER TO WS-NUM-ED
           STRING FUNCTION TRIM(WS-NUM-ED)
                  " business day(s) on calendar "
                  FUNCTION TRIM(DC-CALENDAR)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2100-EXIT.
           EXIT.

      *>   AC-112 - a Bacs entry date that is not a processing day moves
      *>   in the approved direction.  NONE reports but does not move.
       2200-ADJUST SECTION.
       2200-START.
           PERFORM 8000-CHECK-DATE-1
           IF WS-INPUT-BAD
               GO TO 2200-EXIT
           END-IF
           IF DC-DIRECTION = SPACES OR DC-DIRECTION = LOW-VALUES
               MOVE "NEXT" TO DC-DIRECTION
           END-IF
           MOVE FUNCTION UPPER-CASE(DC-DIRECTION) TO DC-DIRECTION
           IF DC-DIRECTION NOT = "NEXT" AND DC-DIRECTION NOT = "PREV"
              AND DC-DIRECTION NOT = "NONE"
               MOVE "DAT0008" TO CA-REASON-CODE
               MOVE "Adjustment direction must be NEXT, PREV or NONE"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 2200-EXIT
           END-IF
           MOVE DC-DATE-1 TO WS-WORK-DATE
           PERFORM 8100-TEST-BUSINESS-DAY
           IF WS-INPUT-BAD
               GO TO 2200-EXIT
           END-IF
           IF DC-IS-BUSINESS = "Y"
               MOVE DC-DATE-1 TO DC-RESULT-DATE
               MOVE 0 TO DC-RESULT-NUM
               MOVE SPACES TO CA-MESSAGE
               STRING "Already a business day on calendar "
                      FUNCTION TRIM(DC-CALENDAR)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 2200-EXIT
           END-IF
           IF DC-DIRECTION = "NONE"
               MOVE DC-DATE-1 TO DC-RESULT-DATE
               MOVE 0 TO DC-RESULT-NUM
               PERFORM 8300-NOT-BUSINESS-MESSAGE
               GO TO 2200-EXIT
           END-IF
           PERFORM 8200-MOVE-TO-BUSINESS
           IF WS-INPUT-BAD
               GO TO 2200-EXIT
           END-IF
           MOVE WS-WORK-DATE TO DC-RESULT-DATE
           MOVE WS-WORK-DATE TO DC-DATE-2
           MOVE "Y" TO DC-ADJUSTED
           MOVE 4 TO CA-RETURN-CODE
           MOVE "DAT0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE DC-RESULT-NUM TO WS-NUM-ED
           STRING "Moved " FUNCTION TRIM(WS-NUM-ED)
                  " day(s) to the "
                  FUNCTION TRIM(DC-DIRECTION)
                  " business day on calendar "
                  FUNCTION TRIM(DC-CALENDAR)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> SHARED INTERNAL ROUTINES
      *>----------------------------------------------------------------
       8000-CHECK-DATE-1 SECTION.
       8000-START.
           IF FUNCTION TEST-DATE-YYYYMMDD(DC-DATE-1) NOT = 0
               MOVE "DAT0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Date " DC-DATE-1
                      " is not a valid calendar date"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       8000-EXIT.
           EXIT.

      *>   sets DC-IS-BUSINESS / DC-IS-WEEKEND / DC-IS-HOLIDAY and the
      *>   day name for WS-WORK-DATE on calendar WS-CAL
       8100-TEST-BUSINESS-DAY SECTION.
       8100-START.
           MOVE "N" TO DC-IS-BUSINESS DC-IS-HOLIDAY DC-IS-WEEKEND
           MOVE SPACES TO DC-HOLIDAY-NAME
           MOVE WS-WORK-DATE TO STD-DATE-N
           MOVE STD-YYYY TO WS-YEAR
      *>   loading a calendar reuses WS-WORK-DATE, so keep the date
      *>   under test safe across the call
           MOVE WS-WORK-DATE TO WS-SAVE-DATE
           PERFORM 7000-FIND-CALENDAR
           MOVE WS-SAVE-DATE TO WS-WORK-DATE
           IF WS-INPUT-BAD
               GO TO 8100-EXIT
           END-IF
           MOVE WS-WORK-DATE TO FNA-DATE
           COMPUTE STD-DOW = FUNCTION WEEKDAY-OF(FNA-DATE)
           MOVE STD-DAY-NAME(STD-DOW) TO DC-DAY-NAME
           MOVE WS-WORK-DATE TO STD-DATE-N
           MOVE STD-MONTH-NAME(STD-MM) TO DC-MONTH-NAME
           IF STD-WEEKEND
               MOVE "Y" TO DC-IS-WEEKEND
           END-IF
           PERFORM 8150-DAY-OF-YEAR
           IF WS-SL-DAY(WS-SLOT-NO, WS-DOY) = "H"
               MOVE "Y" TO DC-IS-HOLIDAY
               PERFORM 8160-HOLIDAY-NAME
           END-IF
           IF DC-IS-WEEKEND = "N" AND DC-IS-HOLIDAY = "N"
               MOVE "Y" TO DC-IS-BUSINESS
           END-IF.
       8100-EXIT.
           EXIT.

       8150-DAY-OF-YEAR SECTION.
       8150-START.
           MOVE WS-WORK-DATE TO STD-DATE-N
           MOVE 1 TO STD-MM
           MOVE 1 TO STD-DD
           MOVE STD-DATE-N TO WS-JAN1
           COMPUTE WS-DOY = FUNCTION INTEGER-OF-DATE(WS-WORK-DATE)
                          - FUNCTION INTEGER-OF-DATE(WS-JAN1) + 1
           MOVE WS-WORK-DATE TO STD-DATE-N.
       8150-EXIT.
           EXIT.

       8160-HOLIDAY-NAME SECTION.
       8160-START.
           PERFORM VARYING WS-J FROM 1 BY 1
               UNTIL WS-J > WS-SL-NAME-CNT(WS-SLOT-NO)
               IF WS-SL-N-DAY(WS-SLOT-NO, WS-J) = WS-DOY
                   MOVE WS-SL-N-TEXT(WS-SLOT-NO, WS-J)
                       TO DC-HOLIDAY-NAME
                   MOVE WS-SL-NAME-CNT(WS-SLOT-NO) TO WS-J
               END-IF
           END-PERFORM.
       8160-EXIT.
           EXIT.

      *>   steps WS-WORK-DATE in DC-DIRECTION until it is a business day
       8200-MOVE-TO-BUSINESS SECTION.
       8200-START.
           IF DC-DIRECTION = "PREV"
               MOVE -1 TO WS-STEP
           ELSE
               MOVE 1 TO WS-STEP
           END-IF
           MOVE 0 TO WS-GUARD
           PERFORM UNTIL DC-IS-BUSINESS = "Y"
                      OR WS-GUARD > 60
                      OR WS-INPUT-BAD
               ADD 1 TO WS-GUARD
               COMPUTE WS-INT = FUNCTION INTEGER-OF-DATE(WS-WORK-DATE)
                                + WS-STEP
               COMPUTE WS-WORK-DATE = FUNCTION DATE-OF-INTEGER(WS-INT)
               PERFORM 8100-TEST-BUSINESS-DAY
           END-PERFORM
           IF WS-GUARD > 60
               MOVE "DAT0006" TO CA-REASON-CODE
               MOVE "No business day found within 60 days"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
           ELSE
               MOVE WS-GUARD TO DC-RESULT-NUM
           END-IF.
       8200-EXIT.
           EXIT.

       8300-NOT-BUSINESS-MESSAGE SECTION.
       8300-START.
           MOVE SPACES TO CA-MESSAGE
           IF DC-IS-HOLIDAY = "Y"
               STRING FUNCTION TRIM(DC-DAY-NAME) " is "
                      FUNCTION TRIM(DC-HOLIDAY-NAME)
                      " on calendar " FUNCTION TRIM(DC-CALENDAR)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               STRING FUNCTION TRIM(DC-DAY-NAME)
                      " is not a business day on calendar "
                      FUNCTION TRIM(DC-CALENDAR)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       8300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> CALENDAR CACHE.  ONE SLOT PER CALENDAR AND YEAR; THE LEAST
      *> RECENTLY USED SLOT IS REPLACED.  CR1890 - READING core.Holiday
      *> ON EVERY DATE TEST MADE THE NIGHTLY AGEING RUN UNUSABLE.
      *>----------------------------------------------------------------
       7000-FIND-CALENDAR SECTION.
       7000-START.
           MOVE 0 TO WS-SLOT-NO
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-CACHE-SLOTS OR WS-SLOT-NO > 0
               IF WS-SL-CAL(WS-I) = WS-CAL
                  AND WS-SL-YEAR(WS-I) = WS-YEAR
                   MOVE WS-I TO WS-SLOT-NO
               END-IF
           END-PERFORM
           IF WS-SLOT-NO > 0
               ADD 1 TO WS-CACHE-HITS
               ADD 1 TO WS-CACHE-TICK
               MOVE WS-CACHE-TICK TO WS-SL-USED(WS-SLOT-NO)
               MOVE WS-SL-SOURCE(WS-SLOT-NO) TO DC-CAL-SOURCE
               MOVE WS-SL-ROWS(WS-SLOT-NO) TO DC-CAL-ROWS
               MOVE WS-CACHE-HITS TO DC-CACHE-HITS
               GO TO 7000-EXIT
           END-IF
           PERFORM 7100-PICK-SLOT
           PERFORM 7200-LOAD-CALENDAR
           ADD 1 TO WS-CACHE-LOADS
           MOVE WS-CACHE-LOADS TO DC-CACHE-LOADS.
       7000-EXIT.
           EXIT.

       7100-PICK-SLOT SECTION.
       7100-START.
           MOVE 1 TO WS-SLOT-NO
           MOVE WS-SL-USED(1) TO WS-OLDEST
           PERFORM VARYING WS-I FROM 2 BY 1
               UNTIL WS-I > WS-CACHE-SLOTS
               IF WS-SL-USED(WS-I) < WS-OLDEST
                   MOVE WS-SL-USED(WS-I) TO WS-OLDEST
                   MOVE WS-I TO WS-SLOT-NO
               END-IF
           END-PERFORM
           ADD 1 TO WS-CACHE-TICK
           MOVE WS-CACHE-TICK TO WS-SL-USED(WS-SLOT-NO)
           MOVE WS-CAL TO WS-SL-CAL(WS-SLOT-NO)
           MOVE WS-YEAR TO WS-SL-YEAR(WS-SLOT-NO)
           MOVE 0 TO WS-SL-ROWS(WS-SLOT-NO)
           MOVE 0 TO WS-SL-NAME-CNT(WS-SLOT-NO)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 366
               MOVE SPACE TO WS-SL-DAY(WS-SLOT-NO, WS-I)
           END-PERFORM.
       7100-EXIT.
           EXIT.

      *>   Bacs does not process on an England and Wales bank holiday,
      *>   so the BACS calendar is loaded from the ENG holiday set.
       7200-LOAD-CALENDAR SECTION.
       7200-START.
           MOVE WS-CAL TO WS-LOAD-CAL
           IF WS-CAL = "BACS"
               MOVE "ENG" TO WS-LOAD-CAL
           END-IF
           IF WS-FORCE-BUILTIN = "N"
               PERFORM 7300-PROBE-TABLES
               IF NOT WS-NO-CAL-TABLE
                   PERFORM 7400-LOAD-FROM-SQL
               END-IF
           END-IF
           IF WS-SL-ROWS(WS-SLOT-NO) = 0
               PERFORM 7500-LOAD-BUILT-IN
           END-IF
           MOVE WS-SL-SOURCE(WS-SLOT-NO) TO DC-CAL-SOURCE
           MOVE WS-SL-ROWS(WS-SLOT-NO) TO DC-CAL-ROWS.
       7200-EXIT.
           EXIT.

      *>   One metadata probe per run decides which core table holds the
      *>   calendar.  COL_LENGTH returns NULL rather than failing if the
      *>   table or column is absent, so this is safe on an empty
      *>   database.  No user data is ever put into SQL text.
       7300-PROBE-TABLES SECTION.
       7300-START.
           IF WS-PROBE-DONE = "Y"
               GO TO 7300-EXIT
           END-IF
           MOVE "Y" TO WS-PROBE-DONE
           MOVE "NO" TO WS-CAL-TABLE
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CASE WHEN COL_LENGTH('core.CalendarDay',"
                  "'CalDate') IS NOT NULL THEN 'CD' "
                  "WHEN COL_LENGTH('core.Holiday','HolidayDate') "
                  "IS NOT NULL THEN 'HD' ELSE 'NO' END"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE "SNGL" TO SQLIO-FUNC
           MOVE 4 TO SQLIO-CURSOR
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-COL-WIDTH(1)
           CALL "SQLIO" USING SQLIO-CA WS-SQL-PROBE
           IF SQLIO-OK
               MOVE WS-SQL-PROBE TO WS-CAL-TABLE
           END-IF.
       7300-EXIT.
           EXIT.

       7400-LOAD-FROM-SQL SECTION.
       7400-START.
           MOVE SPACES TO SQLIO-STMT
           IF WS-USE-CALENDARDAY
      *>       a weekend row carries no holiday name; only named bank
      *>       holidays are cached, weekends come from the day of week
               STRING "SELECT CONVERT(char(8), CalDate, 112), "
                      "HolidayName FROM core.CalendarDay "
                      "WHERE CalendarCode = ? "
                      "AND YEAR(CalDate) = ? "
                      "AND IsBusinessDay = 0 "
                      "AND HolidayName IS NOT NULL "
                      "ORDER BY CalDate"
                   DELIMITED BY SIZE INTO SQLIO-STMT
           ELSE
               STRING "SELECT CONVERT(char(8), HolidayDate, 112), "
                      "ISNULL(Name, 'Bank holiday') "
                      "FROM core.Holiday "
                      "WHERE CalendarCode = ? "
                      "AND YEAR(HolidayDate) = ? "
                      "ORDER BY HolidayDate"
                   DELIMITED BY SIZE INTO SQLIO-STMT
           END-IF
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-LOAD-CAL TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-YEAR TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-COL-WIDTH(1)
           MOVE 28 TO SQLIO-COL-WIDTH(2)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SQL-ROW
           IF SQLIO-ERROR
      *>       a calendar read failure must not stop a date calculation
               MOVE "NO" TO WS-CAL-TABLE
               GO TO 7400-EXIT
           END-IF
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SQL-ROW
           PERFORM UNTIL NOT SQLIO-OK
               MOVE WS-SQL-DATE TO WS-WORK-DATE
               IF FUNCTION TEST-DATE-YYYYMMDD(WS-WORK-DATE) = 0
                   PERFORM 8150-DAY-OF-YEAR
                   MOVE "H" TO WS-SL-DAY(WS-SLOT-NO, WS-DOY)
                   ADD 1 TO WS-SL-ROWS(WS-SLOT-NO)
                   PERFORM 7600-REMEMBER-NAME
               END-IF
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-SQL-ROW
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SQL-ROW
           IF WS-SL-ROWS(WS-SLOT-NO) > 0
               MOVE "SQL" TO WS-SL-SOURCE(WS-SLOT-NO)
           END-IF.
       7400-EXIT.
           EXIT.

       7500-LOAD-BUILT-IN SECTION.
       7500-START.
           MOVE "BUILTIN" TO WS-SL-SOURCE(WS-SLOT-NO)
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > CAL-BUILT-COUNT
               IF CAL-E-CODE(WS-I) = WS-LOAD-CAL(1:3)
                   MOVE CAL-E-DATE(WS-I) TO WS-WORK-DATE
                   MOVE WS-WORK-DATE TO STD-DATE-N
                   IF STD-YYYY = WS-YEAR
                       PERFORM 8150-DAY-OF-YEAR
                       MOVE "H" TO WS-SL-DAY(WS-SLOT-NO, WS-DOY)
                       ADD 1 TO WS-SL-ROWS(WS-SLOT-NO)
                       MOVE CAL-E-NAME(WS-I) TO WS-SQL-NAME
                       PERFORM 7600-REMEMBER-NAME
                   END-IF
               END-IF
           END-PERFORM.
       7500-EXIT.
           EXIT.

       7600-REMEMBER-NAME SECTION.
       7600-START.
           IF WS-SL-NAME-CNT(WS-SLOT-NO) < 20
               ADD 1 TO WS-SL-NAME-CNT(WS-SLOT-NO)
               MOVE WS-DOY
                   TO WS-SL-N-DAY(WS-SLOT-NO,
                                  WS-SL-NAME-CNT(WS-SLOT-NO))
               MOVE WS-SQL-NAME
                   TO WS-SL-N-TEXT(WS-SLOT-NO,
                                   WS-SL-NAME-CNT(WS-SLOT-NO))
           END-IF.
       7600-EXIT.
           EXIT.

       7900-CLEAR-CACHE SECTION.
       7900-START.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-CACHE-SLOTS
               MOVE SPACES TO WS-SL-CAL(WS-I)
               MOVE 0 TO WS-SL-YEAR(WS-I)
               MOVE 0 TO WS-SL-USED(WS-I)
               MOVE 0 TO WS-SL-ROWS(WS-I)
               MOVE 0 TO WS-SL-NAME-CNT(WS-I)
           END-PERFORM
           MOVE "N" TO WS-PROBE-DONE
           MOVE 0 TO WS-CACHE-TICK WS-CACHE-HITS WS-CACHE-LOADS
           MOVE 0 TO DC-CACHE-HITS DC-CACHE-LOADS
           MOVE "Calendar cache cleared" TO CA-MESSAGE.
       7900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "DAT0010" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "DATECALC does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.
