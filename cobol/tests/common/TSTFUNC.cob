      *>================================================================
      *> PROGRAM  : TSTFUNC
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : COMMON SERVICES - UNIT TEST
      *> PURPOSE  : Unit test for STFUNCS, the user-defined FUNCTION
      *>            library (ROUND-AWAY, SIGN-OF, IS-LEAP,
      *>            DAYS-IN-MONTH, END-OF-MONTH, WEEKDAY-OF).
      *> CALLED BY: build\run-tests.ps1 -Layer cobol
      *> INTERFACE: TAP on stdout, RETURN-CODE = number of failures.
      *>
      *> WHY THIS TEST EXISTS.  libcob resolves a user-defined FUNCTION
      *> at run time through cob_resolve_func even when everything else
      *> is bound with -fstatic-call, so nothing in the module archive
      *> refers to STFUNCS and an ordinary link would silently leave it
      *> out; the first CALL would then fail at run time rather than at
      *> build time.  build\build-cobol.ps1 therefore puts the objects
      *> of every source containing a FUNCTION-ID on the link line
      *> explicitly, for the test executables and for STERPCOB.dll
      *> alike (see Func-Objs and Build-Dll).  This suite proves the
      *> arrangement from both sides: the functions are called directly
      *> here, and through DATECALC and RNDALLOC, which are the very
      *> objects the DLL publishes to the .NET client.
      *>
      *> Arguments are passed in the STFNARGS fields, never as literals
      *> or expressions: GnuCOBOL passes user-defined FUNCTION
      *> arguments BY REFERENCE without converting them, so a mismatched
      *> PICTURE is read as raw storage.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2026-10-05 STERP INITIAL - FUNCTION LIBRARY AND ITS LINKAGE
      *>================================================================
      *> REQ: COM-011   AC: AC-006
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSTFUNC.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ROUND-AWAY
           FUNCTION SIGN-OF
           FUNCTION IS-LEAP
           FUNCTION DAYS-IN-MONTH
           FUNCTION END-OF-MONTH
           FUNCTION WEEKDAY-OF.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY STFNARGS.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DC==.
       COPY RNDALLOCCA REPLACING ==DFHCOMMAREA== BY ==WS-RA==.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TSTFUNC" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-ROUND-AWAY
           PERFORM 2000-SIGN-OF
           PERFORM 3000-IS-LEAP
           PERFORM 4000-DAYS-IN-MONTH
           PERFORM 5000-END-OF-MONTH
           PERFORM 6000-WEEKDAY-OF
           PERFORM 7000-THROUGH-LINKABLE
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
      *> 1000 - AC-006.  Money is rounded half away from zero, so that
      *> a half penny never quietly disappears and a credit rounds the
      *> same distance as the debit it reverses.
      *>----------------------------------------------------------------
      *> REQ: COM-011   AC: AC-006
       1000-ROUND-AWAY.
           MOVE "ROUND-AWAY 1.005 to 2 places is 1.01 (AC-006)"
               TO TK-NAME
           MOVE 1.005 TO FNA-VALUE
           MOVE 2 TO FNA-PLACES
           COMPUTE FNA-RESULT =
               FUNCTION ROUND-AWAY(FNA-VALUE, FNA-PLACES)
           MOVE 1.01 TO TK-EXPECTED-N
           MOVE FNA-RESULT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "ROUND-AWAY -1.005 to 2 places is -1.01 (AC-006)"
               TO TK-NAME
           MOVE -1.005 TO FNA-VALUE
           MOVE 2 TO FNA-PLACES
           COMPUTE FNA-RESULT =
               FUNCTION ROUND-AWAY(FNA-VALUE, FNA-PLACES)
           MOVE -1.01 TO TK-EXPECTED-N
           MOVE FNA-RESULT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "ROUND-AWAY 2.675 to 2 places is 2.68" TO TK-NAME
           MOVE 2.675 TO FNA-VALUE
           MOVE 2 TO FNA-PLACES
           COMPUTE FNA-RESULT =
               FUNCTION ROUND-AWAY(FNA-VALUE, FNA-PLACES)
           MOVE 2.68 TO TK-EXPECTED-N
           MOVE FNA-RESULT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "ROUND-AWAY leaves a value already exact" TO TK-NAME
           MOVE 12.34 TO FNA-VALUE
           MOVE 2 TO FNA-PLACES
           COMPUTE FNA-RESULT =
               FUNCTION ROUND-AWAY(FNA-VALUE, FNA-PLACES)
           MOVE 12.34 TO TK-EXPECTED-N
           MOVE FNA-RESULT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "ROUND-AWAY to whole units rounds 0.5 up" TO TK-NAME
           MOVE 0.5 TO FNA-VALUE
           MOVE 0 TO FNA-PLACES
           COMPUTE FNA-RESULT =
               FUNCTION ROUND-AWAY(FNA-VALUE, FNA-PLACES)
           MOVE 1 TO TK-EXPECTED-N
           MOVE FNA-RESULT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "ROUND-AWAY to whole units rounds -0.5 down"
               TO TK-NAME
           MOVE -0.5 TO FNA-VALUE
           MOVE 0 TO FNA-PLACES
           COMPUTE FNA-RESULT =
               FUNCTION ROUND-AWAY(FNA-VALUE, FNA-PLACES)
           MOVE -1 TO TK-EXPECTED-N
           MOVE FNA-RESULT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "ROUND-AWAY treats negative places as none" TO TK-NAME
           MOVE 7.6 TO FNA-VALUE
           MOVE -3 TO FNA-PLACES
           COMPUTE FNA-RESULT =
               FUNCTION ROUND-AWAY(FNA-VALUE, FNA-PLACES)
           MOVE 8 TO TK-EXPECTED-N
           MOVE FNA-RESULT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "ROUND-AWAY rounds a rate to 6 places" TO TK-NAME
           MOVE 1.1234565 TO FNA-VALUE
           MOVE 6 TO FNA-PLACES
           COMPUTE FNA-RESULT =
               FUNCTION ROUND-AWAY(FNA-VALUE, FNA-PLACES)
           MOVE 1.123457 TO TK-EXPECTED-N
           MOVE FNA-RESULT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 2000 - SIGN-OF, used wherever an allocation is performed on
      *> absolute amounts and the sign restored afterwards.
      *>----------------------------------------------------------------
      *> REQ: COM-011
       2000-SIGN-OF.
           MOVE "SIGN-OF of a credit is -1" TO TK-NAME
           MOVE -250.00 TO FNA-VALUE
           MOVE 1 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION SIGN-OF(FNA-VALUE) * -1
           PERFORM TK-ASSERT-NUM

           MOVE "SIGN-OF of zero is 0" TO TK-NAME
           MOVE 0 TO FNA-VALUE
           MOVE 0 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION SIGN-OF(FNA-VALUE)
           PERFORM TK-ASSERT-NUM

           MOVE "SIGN-OF of a debit is +1" TO TK-NAME
           MOVE 0.000001 TO FNA-VALUE
           MOVE 1 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION SIGN-OF(FNA-VALUE)
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 3000 - IS-LEAP, including the century rule that the 1999
      *> remediation was about.
      *>----------------------------------------------------------------
      *> REQ: COM-011
       3000-IS-LEAP.
           MOVE "IS-LEAP 2028 is a leap year" TO TK-NAME
           MOVE 2028 TO FNA-YEAR
           MOVE 1 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION IS-LEAP(FNA-YEAR)
           PERFORM TK-ASSERT-NUM

           MOVE "IS-LEAP 2027 is not a leap year" TO TK-NAME
           MOVE 2027 TO FNA-YEAR
           MOVE 0 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION IS-LEAP(FNA-YEAR)
           PERFORM TK-ASSERT-NUM

           MOVE "IS-LEAP 2000 is a leap year (divisible by 400)"
               TO TK-NAME
           MOVE 2000 TO FNA-YEAR
           MOVE 1 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION IS-LEAP(FNA-YEAR)
           PERFORM TK-ASSERT-NUM

           MOVE "IS-LEAP 1900 is not a leap year (century rule)"
               TO TK-NAME
           MOVE 1900 TO FNA-YEAR
           MOVE 0 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION IS-LEAP(FNA-YEAR)
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 4000 - DAYS-IN-MONTH
      *>----------------------------------------------------------------
      *> REQ: COM-011
       4000-DAYS-IN-MONTH.
           MOVE "DAYS-IN-MONTH February 2028 is 29" TO TK-NAME
           MOVE 2028 TO FNA-YEAR
           MOVE 2 TO FNA-MONTH
           MOVE 29 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N =
               FUNCTION DAYS-IN-MONTH(FNA-YEAR, FNA-MONTH)
           PERFORM TK-ASSERT-NUM

           MOVE "DAYS-IN-MONTH February 2027 is 28" TO TK-NAME
           MOVE 2027 TO FNA-YEAR
           MOVE 2 TO FNA-MONTH
           MOVE 28 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N =
               FUNCTION DAYS-IN-MONTH(FNA-YEAR, FNA-MONTH)
           PERFORM TK-ASSERT-NUM

           MOVE "DAYS-IN-MONTH September 2026 is 30" TO TK-NAME
           MOVE 2026 TO FNA-YEAR
           MOVE 9 TO FNA-MONTH
           MOVE 30 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N =
               FUNCTION DAYS-IN-MONTH(FNA-YEAR, FNA-MONTH)
           PERFORM TK-ASSERT-NUM

           MOVE "DAYS-IN-MONTH December 2026 is 31" TO TK-NAME
           MOVE 2026 TO FNA-YEAR
           MOVE 12 TO FNA-MONTH
           MOVE 31 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N =
               FUNCTION DAYS-IN-MONTH(FNA-YEAR, FNA-MONTH)
           PERFORM TK-ASSERT-NUM

           MOVE "DAYS-IN-MONTH rejects month 13 with 0" TO TK-NAME
           MOVE 2026 TO FNA-YEAR
           MOVE 13 TO FNA-MONTH
           MOVE 0 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N =
               FUNCTION DAYS-IN-MONTH(FNA-YEAR, FNA-MONTH)
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 5000 - END-OF-MONTH, the basis of EOM payment terms.
      *>----------------------------------------------------------------
      *> REQ: COM-011
       5000-END-OF-MONTH.
           MOVE "END-OF-MONTH of 1 Feb 2028 is 29 Feb 2028" TO TK-NAME
           MOVE 20280201 TO FNA-DATE
           MOVE 20280229 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION END-OF-MONTH(FNA-DATE)
           PERFORM TK-ASSERT-NUM

           MOVE "END-OF-MONTH of 15 Feb 2027 is 28 Feb 2027" TO TK-NAME
           MOVE 20270215 TO FNA-DATE
           MOVE 20270228 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION END-OF-MONTH(FNA-DATE)
           PERFORM TK-ASSERT-NUM

           MOVE "END-OF-MONTH of a month end stays put" TO TK-NAME
           MOVE 20261130 TO FNA-DATE
           MOVE 20261130 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION END-OF-MONTH(FNA-DATE)
           PERFORM TK-ASSERT-NUM

      *>     a day that does not exist in the target month is pulled
      *>     back to the month end - the mechanism behind AC-005
           MOVE "END-OF-MONTH pulls 30 Feb 2026 back to 28 Feb 2026"
               TO TK-NAME
           MOVE 20260230 TO FNA-DATE
           MOVE 20260228 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION END-OF-MONTH(FNA-DATE)
           PERFORM TK-ASSERT-NUM

           MOVE "END-OF-MONTH rejects an impossible month with 0"
               TO TK-NAME
           MOVE 20261301 TO FNA-DATE
           MOVE 0 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION END-OF-MONTH(FNA-DATE)
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 6000 - WEEKDAY-OF, 1 Monday .. 7 Sunday.  Used by the business
      *> day arithmetic, so a wrong answer would move every due date.
      *>----------------------------------------------------------------
      *> REQ: COM-011
       6000-WEEKDAY-OF.
           MOVE "WEEKDAY-OF 15 Oct 2026 is a Thursday" TO TK-NAME
           MOVE 20261015 TO FNA-DATE
           MOVE 4 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION WEEKDAY-OF(FNA-DATE)
           PERFORM TK-ASSERT-NUM

           MOVE "WEEKDAY-OF 18 Oct 2026 is a Sunday" TO TK-NAME
           MOVE 20261018 TO FNA-DATE
           MOVE 7 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION WEEKDAY-OF(FNA-DATE)
           PERFORM TK-ASSERT-NUM

           MOVE "WEEKDAY-OF Christmas Day 2026 is a Friday" TO TK-NAME
           MOVE 20261225 TO FNA-DATE
           MOVE 5 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION WEEKDAY-OF(FNA-DATE)
           PERFORM TK-ASSERT-NUM

           MOVE "WEEKDAY-OF 29 Feb 2028 is a Tuesday" TO TK-NAME
           MOVE 20280229 TO FNA-DATE
           MOVE 2 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION WEEKDAY-OF(FNA-DATE)
           PERFORM TK-ASSERT-NUM

           MOVE "WEEKDAY-OF rejects an impossible date with 0"
               TO TK-NAME
           MOVE 20261345 TO FNA-DATE
           MOVE 0 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION WEEKDAY-OF(FNA-DATE)
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 7000 - the same functions reached through LINKable programs.
      *> DATECALC and RNDALLOC are published by STERPCOB.dll, so this
      *> exercises the path the .NET client uses.
      *>----------------------------------------------------------------
      *> REQ: COM-011   AC: AC-006
       7000-THROUGH-LINKABLE.
           INITIALIZE WS-RA
           MOVE "RNDALLOC" TO CA-PROGRAM OF WS-RA
           MOVE "ROUND" TO CA-FUNCTION OF WS-RA
           MOVE "STC" TO CA-COMPANY OF WS-RA
           MOVE 1.005 TO RA-VALUE
           MOVE 2 TO RA-PLACES
           CALL "RNDALLOC" USING WS-RA
           MOVE "RNDALLOC reaches ROUND-AWAY inside the engine"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-RA = 0
               MOVE 1.01 TO TK-EXPECTED-N
               MOVE RA-RESULT TO TK-ACTUAL-N
               PERFORM TK-ASSERT-NUM
           ELSE
               MOVE CA-MESSAGE OF WS-RA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           INITIALIZE WS-DC
           MOVE "DATECALC" TO CA-PROGRAM OF WS-DC
           MOVE "EOMONTH" TO CA-FUNCTION OF WS-DC
           MOVE "STC" TO CA-COMPANY OF WS-DC
           MOVE 20280201 TO DC-DATE-1
           CALL "DATECALC" USING WS-DC
           MOVE "DATECALC reaches END-OF-MONTH inside the engine"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-DC = 0
               MOVE 20280229 TO TK-EXPECTED-N
               MOVE DC-RESULT-DATE TO TK-ACTUAL-N
               PERFORM TK-ASSERT-NUM
           ELSE
               MOVE CA-MESSAGE OF WS-DC TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           INITIALIZE WS-DC
           MOVE "DOW" TO CA-FUNCTION OF WS-DC
           MOVE "STC" TO CA-COMPANY OF WS-DC
           MOVE 20261015 TO DC-DATE-1
           CALL "DATECALC" USING WS-DC
           MOVE "DATECALC reaches WEEKDAY-OF inside the engine"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-DC = 0
               MOVE 4 TO TK-EXPECTED-N
               MOVE DC-RESULT-NUM TO TK-ACTUAL-N
               PERFORM TK-ASSERT-NUM
           ELSE
               MOVE CA-MESSAGE OF WS-DC TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

       COPY TESTKITP.
