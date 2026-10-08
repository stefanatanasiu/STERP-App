      *>================================================================
      *> TDATECAL - unit tests for DATECALC (dates and UK calendars)
      *> Worked numbers:
      *>   AC-112  a Bacs entry date of Friday 25 December 2026 under a
      *>           "next processing day" rule moves to Tuesday 29
      *>           December 2026 (25 and 28 December are bank holidays);
      *>           the processing day is Thursday 24 December and the
      *>           input day Wednesday 23 December; a Scottish holiday
      *>           on Monday 4 January 2027 does not move the England
      *>           and Wales calendar
      *>   AC-005  January invoices and the February month end
      *> The tests ask for the built-in holiday table (DC-CAL-SOURCE =
      *> BUILTIN on entry) so that they are repeatable whatever state
      *> core.CalendarDay happens to be in.
      *>================================================================
      *> REQ: COM-009, COM-026   AC: AC-112
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TDATECAL.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION IS-LEAP
           FUNCTION DAYS-IN-MONTH
           FUNCTION END-OF-MONTH.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY STFNARGS.
       01  WS-OK                       PIC X VALUE "Y".
       01  WS-TEST-DATE                PIC 9(8) VALUE 0.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DC-CA==.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TDATECAL" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-VALIDATION-TESTS
           PERFORM 2000-ARITHMETIC-TESTS
           PERFORM 3000-FORMAT-TESTS
           PERFORM 4000-BUSINESS-DAY-TESTS
           PERFORM 5000-BACS-TESTS
           PERFORM 6000-CACHE-TESTS
           PERFORM 7000-FUNCTION-TESTS
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
       1000-VALIDATION-TESTS.
           MOVE "29 February 2026 is rejected (not a leap year)"
               TO TK-NAME
           MOVE 20260229 TO WS-TEST-DATE
           PERFORM 8000-CALL-VALIDATE
           IF CA-RETURN-CODE OF WS-DC-CA = 8
              AND CA-REASON-CODE OF WS-DC-CA = "DAT0001"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "29 February 2028 is accepted (leap year)" TO TK-NAME
           MOVE 20280229 TO WS-TEST-DATE
           PERFORM 8000-CALL-VALIDATE
           IF CA-RETURN-CODE OF WS-DC-CA = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-DC-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "month 13 is rejected" TO TK-NAME
           MOVE 20261305 TO WS-TEST-DATE
           PERFORM 8000-CALL-VALIDATE
           IF CA-RETURN-CODE OF WS-DC-CA = 8
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "unknown function is rejected with DAT0010" TO TK-NAME
           PERFORM 8900-RESET
           MOVE "SOMEDAY" TO CA-FUNCTION OF WS-DC-CA
           MOVE 20261005 TO DC-DATE-1 OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           IF CA-RETURN-CODE OF WS-DC-CA = 8
              AND CA-REASON-CODE OF WS-DC-CA = "DAT0010"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
       2000-ARITHMETIC-TESTS.
           MOVE "28 February 2026 plus one day is 1 March 2026"
               TO TK-NAME
           PERFORM 8900-RESET
           MOVE "ADDDAYS" TO CA-FUNCTION OF WS-DC-CA
           MOVE 20260228 TO DC-DATE-1 OF WS-DC-CA
           MOVE 1 TO DC-NUMBER OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE 20260301 TO TK-EXPECTED-N
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "28 February 2028 plus one day is 29 February 2028"
               TO TK-NAME
           PERFORM 8900-RESET
           MOVE "ADDDAYS" TO CA-FUNCTION OF WS-DC-CA
           MOVE 20280228 TO DC-DATE-1 OF WS-DC-CA
           MOVE 1 TO DC-NUMBER OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE 20280229 TO TK-EXPECTED-N
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "30 days from 1 October 2026 is 31 October 2026"
               TO TK-NAME
           PERFORM 8900-RESET
           MOVE "ADDDAYS" TO CA-FUNCTION OF WS-DC-CA
           MOVE 20261001 TO DC-DATE-1 OF WS-DC-CA
           MOVE 30 TO DC-NUMBER OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE 20261031 TO TK-EXPECTED-N
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "subtracting days works (1 Jan 2027 minus 1 day)"
               TO TK-NAME
           PERFORM 8900-RESET
           MOVE "ADDDAYS" TO CA-FUNCTION OF WS-DC-CA
           MOVE 20270101 TO DC-DATE-1 OF WS-DC-CA
           MOVE -1 TO DC-NUMBER OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE 20261231 TO TK-EXPECTED-N
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "31 Jan 2027 plus one month is 28 Feb 2027" TO TK-NAME
           PERFORM 8900-RESET
           MOVE "ADDMONTH" TO CA-FUNCTION OF WS-DC-CA
           MOVE 20270131 TO DC-DATE-1 OF WS-DC-CA
           MOVE 1 TO DC-NUMBER OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE 20270228 TO TK-EXPECTED-N
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "31 Jan 2028 plus one month is 29 Feb 2028" TO TK-NAME
           PERFORM 8900-RESET
           MOVE "ADDMONTH" TO CA-FUNCTION OF WS-DC-CA
           MOVE 20280131 TO DC-DATE-1 OF WS-DC-CA
           MOVE 1 TO DC-NUMBER OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE 20280229 TO TK-EXPECTED-N
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "31 Dec 2026 plus 14 months is 28 Feb 2028" TO TK-NAME
           PERFORM 8900-RESET
           MOVE "ADDMONTH" TO CA-FUNCTION OF WS-DC-CA
           MOVE 20261231 TO DC-DATE-1 OF WS-DC-CA
           MOVE 14 TO DC-NUMBER OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE 20280229 TO TK-EXPECTED-N
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "end of month of 5 October 2026 is 31 October"
               TO TK-NAME
           PERFORM 8900-RESET
           MOVE "EOMONTH" TO CA-FUNCTION OF WS-DC-CA
           MOVE 20261005 TO DC-DATE-1 OF WS-DC-CA
           MOVE 0 TO DC-NUMBER OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE 20261031 TO TK-EXPECTED-N
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "end of month after 1 month from 15 Jan 2027 is 28 Feb"
               TO TK-NAME
           PERFORM 8900-RESET
           MOVE "EOMONTH" TO CA-FUNCTION OF WS-DC-CA
           MOVE 20270115 TO DC-DATE-1 OF WS-DC-CA
           MOVE 1 TO DC-NUMBER OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE 20270228 TO TK-EXPECTED-N
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "days between 1 and 31 October 2026 is 30" TO TK-NAME
           PERFORM 8900-RESET
           MOVE "DAYSBTWN" TO CA-FUNCTION OF WS-DC-CA
           MOVE 20261001 TO DC-DATE-1 OF WS-DC-CA
           MOVE 20261031 TO DC-DATE-2 OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE 30 TO TK-EXPECTED-N
           MOVE DC-RESULT-NUM OF WS-DC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "days between is negative when the dates reverse"
               TO TK-NAME
           PERFORM 8900-RESET
           MOVE "DAYSBTWN" TO CA-FUNCTION OF WS-DC-CA
           MOVE 20261031 TO DC-DATE-1 OF WS-DC-CA
           MOVE 20261001 TO DC-DATE-2 OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE -30 TO TK-EXPECTED-N
           MOVE DC-RESULT-NUM OF WS-DC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "5 October 2026 is a Monday" TO TK-NAME
           PERFORM 8900-RESET
           MOVE "DOW" TO CA-FUNCTION OF WS-DC-CA
           MOVE 20261005 TO DC-DATE-1 OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE "Monday" TO TK-EXPECTED-X
           MOVE DC-DAY-NAME OF WS-DC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "25 December 2026 is a Friday" TO TK-NAME
           PERFORM 8900-RESET
           MOVE "DOW" TO CA-FUNCTION OF WS-DC-CA
           MOVE 20261225 TO DC-DATE-1 OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE "Friday" TO TK-EXPECTED-X
           MOVE DC-DAY-NAME OF WS-DC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> COM-024 - 03/04/2027 is never ambiguous because the convention
      *> is stated or the month is shown in words
      *>----------------------------------------------------------------
       3000-FORMAT-TESTS.
           MOVE "format 20261005 as dd/mm/yyyy" TO TK-NAME
           PERFORM 8900-RESET
           MOVE "FORMAT" TO CA-FUNCTION OF WS-DC-CA
           MOVE 20261005 TO DC-DATE-1 OF WS-DC-CA
           MOVE "DDMMYYYY" TO DC-FORMAT-OUT OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE "05/10/2026" TO TK-EXPECTED-X
           MOVE DC-TEXT-OUT OF WS-DC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "format 20270403 as yyyy-mm-dd" TO TK-NAME
           PERFORM 8900-RESET
           MOVE "FORMAT" TO CA-FUNCTION OF WS-DC-CA
           MOVE 20270403 TO DC-DATE-1 OF WS-DC-CA
           MOVE "ISO" TO DC-FORMAT-OUT OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE "2027-04-03" TO TK-EXPECTED-X
           MOVE DC-TEXT-OUT OF WS-DC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "format names the month in words (April)" TO TK-NAME
           MOVE "April" TO TK-EXPECTED-X
           MOVE DC-MONTH-NAME OF WS-DC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "parse 05/10/2026 as a UK date" TO TK-NAME
           PERFORM 8900-RESET
           MOVE "PARSE" TO CA-FUNCTION OF WS-DC-CA
           MOVE "05/10/2026" TO DC-TEXT-IN OF WS-DC-CA
           MOVE "DDMMYYYY" TO DC-FORMAT-IN OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE 20261005 TO TK-EXPECTED-N
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "parse 2026-10-05 as an ISO date" TO TK-NAME
           PERFORM 8900-RESET
           MOVE "PARSE" TO CA-FUNCTION OF WS-DC-CA
           MOVE "2026-10-05" TO DC-TEXT-IN OF WS-DC-CA
           MOVE "ISO" TO DC-FORMAT-IN OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE 20261005 TO TK-EXPECTED-N
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "parse 31/02/2027 is rejected as not a real date"
               TO TK-NAME
           PERFORM 8900-RESET
           MOVE "PARSE" TO CA-FUNCTION OF WS-DC-CA
           MOVE "31/02/2027" TO DC-TEXT-IN OF WS-DC-CA
           MOVE "DDMMYYYY" TO DC-FORMAT-IN OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           IF CA-RETURN-CODE OF WS-DC-CA = 8
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "parse of rubbish is rejected with DAT0004" TO TK-NAME
           PERFORM 8900-RESET
           MOVE "PARSE" TO CA-FUNCTION OF WS-DC-CA
           MOVE "NEXT WEEK" TO DC-TEXT-IN OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           IF CA-RETURN-CODE OF WS-DC-CA = 8
              AND CA-REASON-CODE OF WS-DC-CA = "DAT0004"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> COM-026 - the right calendar is applied to each date rule
      *>----------------------------------------------------------------
       4000-BUSINESS-DAY-TESTS.
           MOVE "25 December 2026 is not an ENG business day"
               TO TK-NAME
           MOVE 20261225 TO DC-DATE-1 OF WS-DC-CA
           MOVE "ENG" TO DC-CALENDAR OF WS-DC-CA
           PERFORM 8100-CALL-ISBUSDAY
           IF DC-IS-BUSINESS OF WS-DC-CA = "N"
              AND DC-IS-HOLIDAY OF WS-DC-CA = "Y"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-DC-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "28 December 2026 is the Boxing Day substitute"
               TO TK-NAME
           MOVE 20261228 TO DC-DATE-1 OF WS-DC-CA
           MOVE "ENG" TO DC-CALENDAR OF WS-DC-CA
           PERFORM 8100-CALL-ISBUSDAY
           IF DC-IS-BUSINESS OF WS-DC-CA = "N"
              AND DC-IS-HOLIDAY OF WS-DC-CA = "Y"
              AND DC-IS-WEEKEND OF WS-DC-CA = "N"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-DC-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "29 December 2026 is a business day" TO TK-NAME
           MOVE 20261229 TO DC-DATE-1 OF WS-DC-CA
           MOVE "ENG" TO DC-CALENDAR OF WS-DC-CA
           PERFORM 8100-CALL-ISBUSDAY
           IF DC-IS-BUSINESS OF WS-DC-CA = "Y"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-DC-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "26 December 2026 is a Saturday, not a business day"
               TO TK-NAME
           MOVE 20261226 TO DC-DATE-1 OF WS-DC-CA
           MOVE "ENG" TO DC-CALENDAR OF WS-DC-CA
           PERFORM 8100-CALL-ISBUSDAY
           IF DC-IS-BUSINESS OF WS-DC-CA = "N"
              AND DC-IS-WEEKEND OF WS-DC-CA = "Y"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "AC-112 4 January 2027 is a Scottish holiday"
               TO TK-NAME
           MOVE 20270104 TO DC-DATE-1 OF WS-DC-CA
           MOVE "SCT" TO DC-CALENDAR OF WS-DC-CA
           PERFORM 8100-CALL-ISBUSDAY
           IF DC-IS-BUSINESS OF WS-DC-CA = "N"
              AND DC-IS-HOLIDAY OF WS-DC-CA = "Y"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-DC-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "AC-112 the Scottish holiday leaves ENG unaffected"
               TO TK-NAME
           MOVE 20270104 TO DC-DATE-1 OF WS-DC-CA
           MOVE "ENG" TO DC-CALENDAR OF WS-DC-CA
           PERFORM 8100-CALL-ISBUSDAY
           IF DC-IS-BUSINESS OF WS-DC-CA = "Y"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-DC-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "2 January 2026 is a Scottish bank holiday" TO TK-NAME
           MOVE 20260102 TO DC-DATE-1 OF WS-DC-CA
           MOVE "SCT" TO DC-CALENDAR OF WS-DC-CA
           PERFORM 8100-CALL-ISBUSDAY
           IF DC-IS-BUSINESS OF WS-DC-CA = "N"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "2 January 2026 is an ordinary English working Friday"
               TO TK-NAME
           MOVE 20260102 TO DC-DATE-1 OF WS-DC-CA
           MOVE "ENG" TO DC-CALENDAR OF WS-DC-CA
           PERFORM 8100-CALL-ISBUSDAY
           IF DC-IS-BUSINESS OF WS-DC-CA = "Y"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "6 April 2026 Easter Monday is not kept in Scotland"
               TO TK-NAME
           MOVE 20260406 TO DC-DATE-1 OF WS-DC-CA
           MOVE "SCT" TO DC-CALENDAR OF WS-DC-CA
           PERFORM 8100-CALL-ISBUSDAY
           IF DC-IS-BUSINESS OF WS-DC-CA = "Y"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "17 March 2026 St Patrick's Day is kept in NIR only"
               TO TK-NAME
           MOVE "Y" TO WS-OK
           MOVE 20260317 TO DC-DATE-1 OF WS-DC-CA
           MOVE "NIR" TO DC-CALENDAR OF WS-DC-CA
           PERFORM 8100-CALL-ISBUSDAY
           IF DC-IS-BUSINESS OF WS-DC-CA NOT = "N"
               MOVE "N" TO WS-OK
           END-IF
           MOVE 20260317 TO DC-DATE-1 OF WS-DC-CA
           MOVE "ENG" TO DC-CALENDAR OF WS-DC-CA
           PERFORM 8100-CALL-ISBUSDAY
           IF DC-IS-BUSINESS OF WS-DC-CA NOT = "Y"
               MOVE "N" TO WS-OK
           END-IF
           IF WS-OK = "Y" PERFORM TK-PASS ELSE PERFORM TK-FAIL END-IF

           MOVE "13 July 2026 is the Boyne substitute in NIR"
               TO TK-NAME
           MOVE 20260713 TO DC-DATE-1 OF WS-DC-CA
           MOVE "NIR" TO DC-CALENDAR OF WS-DC-CA
           PERFORM 8100-CALL-ISBUSDAY
           IF DC-IS-BUSINESS OF WS-DC-CA = "N"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "30 November 2026 St Andrew's Day is kept in Scotland"
               TO TK-NAME
           MOVE 20261130 TO DC-DATE-1 OF WS-DC-CA
           MOVE "SCT" TO DC-CALENDAR OF WS-DC-CA
           PERFORM 8100-CALL-ISBUSDAY
           IF DC-IS-BUSINESS OF WS-DC-CA = "N"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "the holiday name is reported" TO TK-NAME
           MOVE 20261225 TO DC-DATE-1 OF WS-DC-CA
           MOVE "ENG" TO DC-CALENDAR OF WS-DC-CA
           PERFORM 8100-CALL-ISBUSDAY
           MOVE "Christmas Day" TO TK-EXPECTED-X
           MOVE DC-HOLIDAY-NAME OF WS-DC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "the default calendar is England and Wales" TO TK-NAME
           PERFORM 8900-RESET
           MOVE "ISBUSDAY" TO CA-FUNCTION OF WS-DC-CA
           MOVE "BUILTIN" TO DC-CAL-SOURCE OF WS-DC-CA
           MOVE 20270104 TO DC-DATE-1 OF WS-DC-CA
           MOVE SPACES TO DC-CALENDAR OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           IF DC-CALENDAR OF WS-DC-CA = "ENG"
              AND DC-IS-BUSINESS OF WS-DC-CA = "Y"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> AC-112 Bacs processing calendar
      *>----------------------------------------------------------------
       5000-BACS-TESTS.
           MOVE "AC-112 Bacs entry 25 Dec 2026 moves to 29 Dec 2026"
               TO TK-NAME
           PERFORM 8900-RESET
           MOVE "ADJUST" TO CA-FUNCTION OF WS-DC-CA
           MOVE "BUILTIN" TO DC-CAL-SOURCE OF WS-DC-CA
           MOVE 20261225 TO DC-DATE-1 OF WS-DC-CA
           MOVE "BACS" TO DC-CALENDAR OF WS-DC-CA
           MOVE "NEXT" TO DC-DIRECTION OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE 20261229 TO TK-EXPECTED-N
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "the move is reported as a warning, not silently"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-DC-CA = 4
              AND DC-ADJUSTED OF WS-DC-CA = "Y"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "AC-112 processing day is Thursday 24 December 2026"
               TO TK-NAME
           PERFORM 8900-RESET
           MOVE "ADDBUS" TO CA-FUNCTION OF WS-DC-CA
           MOVE "BUILTIN" TO DC-CAL-SOURCE OF WS-DC-CA
           MOVE 20261229 TO DC-DATE-1 OF WS-DC-CA
           MOVE "BACS" TO DC-CALENDAR OF WS-DC-CA
           MOVE -1 TO DC-NUMBER OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE 20261224 TO TK-EXPECTED-N
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-112 input day is Wednesday 23 December 2026"
               TO TK-NAME
           PERFORM 8900-RESET
           MOVE "ADDBUS" TO CA-FUNCTION OF WS-DC-CA
           MOVE "BUILTIN" TO DC-CAL-SOURCE OF WS-DC-CA
           MOVE 20261229 TO DC-DATE-1 OF WS-DC-CA
           MOVE "BACS" TO DC-CALENDAR OF WS-DC-CA
           MOVE -2 TO DC-NUMBER OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE 20261223 TO TK-EXPECTED-N
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "previous business day before Christmas Day 2026"
               TO TK-NAME
           PERFORM 8900-RESET
           MOVE "ADJUST" TO CA-FUNCTION OF WS-DC-CA
           MOVE "BUILTIN" TO DC-CAL-SOURCE OF WS-DC-CA
           MOVE 20261225 TO DC-DATE-1 OF WS-DC-CA
           MOVE "BACS" TO DC-CALENDAR OF WS-DC-CA
           MOVE "PREV" TO DC-DIRECTION OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE 20261224 TO TK-EXPECTED-N
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "direction NONE reports but does not move the date"
               TO TK-NAME
           PERFORM 8900-RESET
           MOVE "ADJUST" TO CA-FUNCTION OF WS-DC-CA
           MOVE "BUILTIN" TO DC-CAL-SOURCE OF WS-DC-CA
           MOVE 20261225 TO DC-DATE-1 OF WS-DC-CA
           MOVE "BACS" TO DC-CALENDAR OF WS-DC-CA
           MOVE "NONE" TO DC-DIRECTION OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           IF DC-RESULT-DATE OF WS-DC-CA = 20261225
              AND DC-IS-BUSINESS OF WS-DC-CA = "N"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "a date that is already a business day is not moved"
               TO TK-NAME
           PERFORM 8900-RESET
           MOVE "ADJUST" TO CA-FUNCTION OF WS-DC-CA
           MOVE "BUILTIN" TO DC-CAL-SOURCE OF WS-DC-CA
           MOVE 20261224 TO DC-DATE-1 OF WS-DC-CA
           MOVE "BACS" TO DC-CALENDAR OF WS-DC-CA
           MOVE "NEXT" TO DC-DIRECTION OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           IF DC-RESULT-DATE OF WS-DC-CA = 20261224
              AND CA-RETURN-CODE OF WS-DC-CA = 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "one business day after Fri 24 Dec 2027 is Wed 29 Dec"
               TO TK-NAME
           PERFORM 8900-RESET
           MOVE "ADDBUS" TO CA-FUNCTION OF WS-DC-CA
           MOVE "BUILTIN" TO DC-CAL-SOURCE OF WS-DC-CA
           MOVE 20271224 TO DC-DATE-1 OF WS-DC-CA
           MOVE "ENG" TO DC-CALENDAR OF WS-DC-CA
           MOVE 1 TO DC-NUMBER OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE 20271229 TO TK-EXPECTED-N
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "new year 2028: ENG resumes on 4 Jan, Scotland on 5"
               TO TK-NAME
           MOVE "Y" TO WS-OK
           PERFORM 8900-RESET
           MOVE "ADDBUS" TO CA-FUNCTION OF WS-DC-CA
           MOVE "BUILTIN" TO DC-CAL-SOURCE OF WS-DC-CA
           MOVE 20271231 TO DC-DATE-1 OF WS-DC-CA
           MOVE "ENG" TO DC-CALENDAR OF WS-DC-CA
           MOVE 1 TO DC-NUMBER OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           IF DC-RESULT-DATE OF WS-DC-CA NOT = 20280104
               MOVE "N" TO WS-OK
           END-IF
           PERFORM 8900-RESET
           MOVE "ADDBUS" TO CA-FUNCTION OF WS-DC-CA
           MOVE "BUILTIN" TO DC-CAL-SOURCE OF WS-DC-CA
           MOVE 20271231 TO DC-DATE-1 OF WS-DC-CA
           MOVE "SCT" TO DC-CALENDAR OF WS-DC-CA
           MOVE 1 TO DC-NUMBER OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           IF DC-RESULT-DATE OF WS-DC-CA NOT = 20280105
               MOVE "N" TO WS-OK
               MOVE CA-MESSAGE OF WS-DC-CA TO TK-DETAIL
           END-IF
           IF WS-OK = "Y" PERFORM TK-PASS ELSE PERFORM TK-FAIL END-IF

           MOVE "five business days from Mon 5 Oct 2026 is Mon 12 Oct"
               TO TK-NAME
           PERFORM 8900-RESET
           MOVE "ADDBUS" TO CA-FUNCTION OF WS-DC-CA
           MOVE "BUILTIN" TO DC-CAL-SOURCE OF WS-DC-CA
           MOVE 20261005 TO DC-DATE-1 OF WS-DC-CA
           MOVE "ENG" TO DC-CALENDAR OF WS-DC-CA
           MOVE 5 TO DC-NUMBER OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE 20261012 TO TK-EXPECTED-N
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
       6000-CACHE-TESTS.
           MOVE "the calendar cache can be cleared" TO TK-NAME
           PERFORM 8900-RESET
           MOVE "CACHECLR" TO CA-FUNCTION OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           IF CA-RETURN-CODE OF WS-DC-CA = 0
              AND DC-CACHE-HITS OF WS-DC-CA = 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "the first use of a calendar year loads it" TO TK-NAME
           MOVE 20261005 TO DC-DATE-1 OF WS-DC-CA
           MOVE "ENG" TO DC-CALENDAR OF WS-DC-CA
           PERFORM 8100-CALL-ISBUSDAY
           IF DC-CACHE-LOADS OF WS-DC-CA = 1
              AND DC-CACHE-HITS OF WS-DC-CA = 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "the second use of the same year is a cache hit"
               TO TK-NAME
           MOVE 20261006 TO DC-DATE-1 OF WS-DC-CA
           MOVE "ENG" TO DC-CALENDAR OF WS-DC-CA
           PERFORM 8100-CALL-ISBUSDAY
           IF DC-CACHE-HITS OF WS-DC-CA = 1
              AND DC-CACHE-LOADS OF WS-DC-CA = 1
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "another calendar for the same year loads again"
               TO TK-NAME
           MOVE 20261006 TO DC-DATE-1 OF WS-DC-CA
           MOVE "SCT" TO DC-CALENDAR OF WS-DC-CA
           PERFORM 8100-CALL-ISBUSDAY
           IF DC-CACHE-LOADS OF WS-DC-CA = 2
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "the built-in table holds eight ENG holidays for 2026"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE 20261005 TO DC-DATE-1 OF WS-DC-CA
           MOVE "ENG" TO DC-CALENDAR OF WS-DC-CA
           PERFORM 8100-CALL-ISBUSDAY
           MOVE DC-CAL-ROWS OF WS-DC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
       7000-FUNCTION-TESTS.
           MOVE "FUNCTION IS-LEAP: 2028 yes, 2027 no" TO TK-NAME
           MOVE "Y" TO WS-OK
           MOVE 2028 TO FNA-YEAR
           IF FUNCTION IS-LEAP(FNA-YEAR) NOT = 1
               MOVE "N" TO WS-OK
           END-IF
           MOVE 2027 TO FNA-YEAR
           IF FUNCTION IS-LEAP(FNA-YEAR) NOT = 0
               MOVE "N" TO WS-OK
           END-IF
           IF WS-OK = "Y" PERFORM TK-PASS ELSE PERFORM TK-FAIL END-IF

           MOVE "FUNCTION IS-LEAP: 2000 yes, 1900 no (century rule)"
               TO TK-NAME
           MOVE "Y" TO WS-OK
           MOVE 2000 TO FNA-YEAR
           IF FUNCTION IS-LEAP(FNA-YEAR) NOT = 1
               MOVE "N" TO WS-OK
           END-IF
           MOVE 1900 TO FNA-YEAR
           IF FUNCTION IS-LEAP(FNA-YEAR) NOT = 0
               MOVE "N" TO WS-OK
           END-IF
           IF WS-OK = "Y" PERFORM TK-PASS ELSE PERFORM TK-FAIL END-IF

           MOVE "FUNCTION DAYS-IN-MONTH February 2028 is 29" TO TK-NAME
           MOVE 2028 TO FNA-YEAR
           MOVE 2 TO FNA-MONTH
           MOVE 29 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N =
               FUNCTION DAYS-IN-MONTH(FNA-YEAR, FNA-MONTH)
           PERFORM TK-ASSERT-NUM

           MOVE "FUNCTION DAYS-IN-MONTH February 2027 is 28" TO TK-NAME
           MOVE 2027 TO FNA-YEAR
           MOVE 2 TO FNA-MONTH
           MOVE 28 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N =
               FUNCTION DAYS-IN-MONTH(FNA-YEAR, FNA-MONTH)
           PERFORM TK-ASSERT-NUM

           MOVE "FUNCTION DAYS-IN-MONTH rejects month 13" TO TK-NAME
           MOVE 2027 TO FNA-YEAR
           MOVE 13 TO FNA-MONTH
           MOVE 0 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N =
               FUNCTION DAYS-IN-MONTH(FNA-YEAR, FNA-MONTH)
           PERFORM TK-ASSERT-NUM

           MOVE "FUNCTION END-OF-MONTH of 1 Feb 2028 is 29 Feb 2028"
               TO TK-NAME
           MOVE 20280201 TO FNA-DATE
           MOVE 20280229 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION END-OF-MONTH(FNA-DATE)
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
       8000-CALL-VALIDATE.
           PERFORM 8900-RESET
           MOVE "VALIDATE" TO CA-FUNCTION OF WS-DC-CA
           MOVE WS-TEST-DATE TO DC-DATE-1 OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA.

       8100-CALL-ISBUSDAY.
           MOVE "ISBUSDAY" TO CA-FUNCTION OF WS-DC-CA
           MOVE "BUILTIN" TO DC-CAL-SOURCE OF WS-DC-CA
           MOVE 0 TO CA-RETURN-CODE OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA.

       8900-RESET.
           INITIALIZE WS-DC-CA.

       COPY TESTKITP.
