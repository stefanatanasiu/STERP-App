      *>================================================================
      *> DATECALCCA - COMMAREA for DATECALC (date and calendar service)
      *> CA-FUNCTION values
      *>   VALIDATE  check DC-DATE-1          -> DC-RESULT-NUM 0 = valid
      *>   ADDDAYS   DC-DATE-1 + DC-NUMBER calendar days
      *>   ADDMONTH  DC-DATE-1 + DC-NUMBER calendar months; a day that
      *>             does not exist in the target month becomes that
      *>             month's final day (spec 01 section 9)
      *>   EOMONTH   last day of the month DC-NUMBER months after
      *>             DC-DATE-1 (DC-NUMBER 0 = same month)
      *>   DOW       day of week  -> DC-RESULT-NUM 1 Mon .. 7 Sun,
      *>             DC-DAY-NAME, DC-MONTH-NAME
      *>   DAYSBTWN  DC-DATE-2 minus DC-DATE-1 in days -> DC-RESULT-NUM
      *>   FORMAT    DC-DATE-1 -> DC-TEXT-OUT using DC-FORMAT-OUT
      *>   PARSE     DC-TEXT-IN using DC-FORMAT-IN -> DC-RESULT-DATE
      *>   ISBUSDAY  business day test on DC-CALENDAR
      *>   ADDBUS    add DC-NUMBER business days (may be negative)
      *>   ADJUST    move to the DC-DIRECTION business day if the date
      *>             is not one (NEXT / PREV / NONE)
      *>   CACHECLR  empty the calendar cache (maintenance / tests)
      *> DC-FORMAT-IN / DC-FORMAT-OUT: "YYYYMMDD", "DDMMYYYY" (shown as
      *>   dd/mm/yyyy) or "ISO" (yyyy-mm-dd).
      *> DC-CALENDAR: ENG (default), SCT, NIR, BACS - see COM-026.
      *> RETURN CODES 00 ok, 04 adjusted, 08 invalid (DAT0001..DAT0009),
      *>   12 severe (SQL).  DC-CAL-SOURCE reports SQL or BUILTIN.
      *>================================================================
      *> REQ: COM-009, COM-026   AC: AC-112
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  DC-BODY.
               10  DC-DATE-1           PIC 9(8).
               10  DC-DATE-2           PIC 9(8).
               10  DC-RESULT-DATE      PIC 9(8).
               10  DC-NUMBER           PIC S9(9) COMP-5.
               10  DC-RESULT-NUM       PIC S9(9) COMP-5.
               10  DC-CALENDAR         PIC X(10).
               10  DC-DIRECTION        PIC X(4).
               10  DC-TEXT-IN          PIC X(10).
               10  DC-TEXT-OUT         PIC X(10).
               10  DC-FORMAT-IN        PIC X(8).
               10  DC-FORMAT-OUT       PIC X(8).
               10  DC-DAY-NAME         PIC X(9).
               10  DC-MONTH-NAME       PIC X(9).
               10  DC-IS-BUSINESS      PIC X.
               10  DC-IS-HOLIDAY       PIC X.
               10  DC-IS-WEEKEND       PIC X.
               10  DC-ADJUSTED         PIC X.
               10  DC-HOLIDAY-NAME     PIC X(28).
               10  DC-CAL-SOURCE       PIC X(8).
               10  DC-CAL-ROWS         PIC S9(9) COMP-5.
               10  DC-CACHE-HITS       PIC S9(9) COMP-5.
               10  DC-CACHE-LOADS      PIC S9(9) COMP-5.
