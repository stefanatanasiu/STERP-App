      *>================================================================
      *> SOURCE     : STFUNCS
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COMMON SERVICES
      *> PURPOSE    : LIBRARY OF USER-DEFINED FUNCTIONS SHARED BY THE
      *>              WHOLE ENGINE.  THESE ARE NOT LINKABLE PROGRAMS -
      *>              THEY CARRY NO COMMAREA AND ARE NOT IN THE PROGRAM
      *>              DIRECTORY OF STERPCOB.DLL.  THEY ARE COMPILED INTO
      *>              libst_common.a AND RESOLVED STATICALLY, SO THEY
      *>              WORK IN THE DLL, IN BATCH EXES AND IN TEST EXES.
      *>
      *>              ROUND-AWAY    (VALUE, PLACES)  -> ROUNDED VALUE
      *>              SIGN-OF       (VALUE)          -> -1 / 0 / +1
      *>              IS-LEAP       (YEAR)           ->  1 / 0
      *>              DAYS-IN-MONTH (YEAR, MONTH)    -> 28..31, 0 BAD
      *>              END-OF-MONTH  (YYYYMMDD)       -> YYYYMMDD, 0 BAD
      *>              WEEKDAY-OF   (YYYYMMDD)       -> 1 MON..7 SUN
      *>
      *> CALLING    : GNUCOBOL PASSES FUNCTION ARGUMENTS BY REFERENCE
      *>              WITHOUT CONVERSION.  CALLERS MUST COPY STFNARGS
      *>              AND PASS ITS FIELDS - SEE THAT COPYBOOK.
      *>              EVERY CALLER NEEDS, IN ITS CONFIGURATION SECTION:
      *>                  REPOSITORY.
      *>                      FUNCTION ROUND-AWAY
      *>                      FUNCTION SIGN-OF.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2012-04-23 ANW  CR2641 ROUNDING MOVED OUT OF 14 COPY-PASTED
      *>                 PARAGRAPHS INTO ONE (AUDIT FINDING 2011/3)
      *> 2026-10-05 STERP DATE HELPERS ADDED FOR DATECALC / TERMCALC
      *>================================================================
      *> REQ: COM-011, COM-009

      *>----------------------------------------------------------------
      *> ROUND-AWAY - ROUND TO "PLACES" DECIMALS, EXACT HALVES AWAY FROM
      *> ZERO (SPEC 01 SECTION 8).  PLACES IS CLAMPED TO 0..8.  THE
      *> VALUE IS SCALED BY A TABLE POWER OF TEN RATHER THAN BY THE **
      *> OPERATOR, WHICH WOULD GO THROUGH FLOATING POINT.
      *>----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       FUNCTION-ID. ROUND-AWAY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-POWERS.
           05  FILLER                  PIC 9(9) VALUE 1.
           05  FILLER                  PIC 9(9) VALUE 10.
           05  FILLER                  PIC 9(9) VALUE 100.
           05  FILLER                  PIC 9(9) VALUE 1000.
           05  FILLER                  PIC 9(9) VALUE 10000.
           05  FILLER                  PIC 9(9) VALUE 100000.
           05  FILLER                  PIC 9(9) VALUE 1000000.
           05  FILLER                  PIC 9(9) VALUE 10000000.
           05  FILLER                  PIC 9(9) VALUE 100000000.
       01  WS-POWER-TAB REDEFINES WS-POWERS.
           05  WS-POWER                PIC 9(9) OCCURS 9 TIMES.
       01  WS-P                        PIC S9(4) VALUE 0.
       01  WS-SCALED                   PIC S9(28) VALUE 0.
       LINKAGE SECTION.
       01  LK-VALUE                    PIC S9(18)V9(12).
       01  LK-PLACES                   PIC S9(4).
       01  LK-RESULT                   PIC S9(18)V9(12).
       PROCEDURE DIVISION USING LK-VALUE LK-PLACES
                          RETURNING LK-RESULT.
       R-MAIN.
           MOVE LK-PLACES TO WS-P
           IF WS-P < 0
               MOVE 0 TO WS-P
           END-IF
           IF WS-P > 8
               MOVE 8 TO WS-P
           END-IF
           IF LK-VALUE < 0
               COMPUTE WS-SCALED =
                   LK-VALUE * WS-POWER(WS-P + 1) - 0.5
           ELSE
               COMPUTE WS-SCALED =
                   LK-VALUE * WS-POWER(WS-P + 1) + 0.5
           END-IF
           COMPUTE LK-RESULT = WS-SCALED / WS-POWER(WS-P + 1)
           GOBACK.
       END FUNCTION ROUND-AWAY.

      *>----------------------------------------------------------------
      *> SIGN-OF - CLASSIC THREE WAY SIGN.  USED WHEREVER AN ALLOCATION
      *> OR A DISCOUNT HAS TO BE PERFORMED ON ABSOLUTE AMOUNTS AND THE
      *> SIGN RESTORED AFTERWARDS.
      *>----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       FUNCTION-ID. SIGN-OF.
       DATA DIVISION.
       LINKAGE SECTION.
       01  LK-VALUE                    PIC S9(18)V9(12).
       01  LK-RESULT                   PIC S9(4).
       PROCEDURE DIVISION USING LK-VALUE RETURNING LK-RESULT.
       S-MAIN.
           EVALUATE TRUE
               WHEN LK-VALUE < 0   MOVE -1 TO LK-RESULT
               WHEN LK-VALUE > 0   MOVE +1 TO LK-RESULT
               WHEN OTHER          MOVE  0 TO LK-RESULT
           END-EVALUATE
           GOBACK.
       END FUNCTION SIGN-OF.

      *>----------------------------------------------------------------
      *> IS-LEAP - GREGORIAN LEAP YEAR TEST.  1 = LEAP, 0 = COMMON.
      *> (THE 1900 / 2000 CENTURY RULE CAUGHT THE OLD AGEING SUITE OUT
      *> IN FEBRUARY 2000 - CHANGE REQUEST CR1288.)
      *>----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       FUNCTION-ID. IS-LEAP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-Y                        PIC S9(9) VALUE 0.
       LINKAGE SECTION.
       01  LK-YEAR                     PIC S9(4).
       01  LK-RESULT                   PIC S9(4).
       PROCEDURE DIVISION USING LK-YEAR RETURNING LK-RESULT.
       L-MAIN.
           MOVE 0 TO LK-RESULT
           MOVE LK-YEAR TO WS-Y
           IF WS-Y < 1601 OR WS-Y > 9999
               GOBACK
           END-IF
           IF FUNCTION MOD(WS-Y, 400) = 0
               MOVE 1 TO LK-RESULT
           ELSE
               IF FUNCTION MOD(WS-Y, 100) NOT = 0
                  AND FUNCTION MOD(WS-Y, 4) = 0
                   MOVE 1 TO LK-RESULT
               END-IF
           END-IF
           GOBACK.
       END FUNCTION IS-LEAP.

      *>----------------------------------------------------------------
      *> DAYS-IN-MONTH - LENGTH OF A CALENDAR MONTH, 0 WHEN THE MONTH OR
      *> YEAR IS OUT OF RANGE.  THE LEAP RULE IS REPEATED HERE RATHER
      *> THAN CALLING IS-LEAP, TO KEEP EACH FUNCTION SELF CONTAINED.
      *>----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       FUNCTION-ID. DAYS-IN-MONTH.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-LENGTHS.
           05  FILLER                  PIC 9(2) VALUE 31.
           05  FILLER                  PIC 9(2) VALUE 28.
           05  FILLER                  PIC 9(2) VALUE 31.
           05  FILLER                  PIC 9(2) VALUE 30.
           05  FILLER                  PIC 9(2) VALUE 31.
           05  FILLER                  PIC 9(2) VALUE 30.
           05  FILLER                  PIC 9(2) VALUE 31.
           05  FILLER                  PIC 9(2) VALUE 31.
           05  FILLER                  PIC 9(2) VALUE 30.
           05  FILLER                  PIC 9(2) VALUE 31.
           05  FILLER                  PIC 9(2) VALUE 30.
           05  FILLER                  PIC 9(2) VALUE 31.
       01  WS-LEN-TAB REDEFINES WS-LENGTHS.
           05  WS-LEN                  PIC 9(2) OCCURS 12 TIMES.
       01  WS-Y                        PIC S9(9) VALUE 0.
       01  WS-M                        PIC S9(4) VALUE 0.
       LINKAGE SECTION.
       01  LK-YEAR                     PIC S9(4).
       01  LK-MONTH                    PIC S9(4).
       01  LK-RESULT                   PIC S9(4).
       PROCEDURE DIVISION USING LK-YEAR LK-MONTH RETURNING LK-RESULT.
       D-MAIN.
           MOVE 0 TO LK-RESULT
           MOVE LK-YEAR TO WS-Y
           MOVE LK-MONTH TO WS-M
           IF WS-Y < 1601 OR WS-Y > 9999
               GOBACK
           END-IF
           IF WS-M < 1 OR WS-M > 12
               GOBACK
           END-IF
           MOVE WS-LEN(WS-M) TO LK-RESULT
           IF WS-M = 2
               IF FUNCTION MOD(WS-Y, 400) = 0
                   MOVE 29 TO LK-RESULT
               ELSE
                   IF FUNCTION MOD(WS-Y, 100) NOT = 0
                      AND FUNCTION MOD(WS-Y, 4) = 0
                       MOVE 29 TO LK-RESULT
                   END-IF
               END-IF
           END-IF
           GOBACK.
       END FUNCTION DAYS-IN-MONTH.

      *>----------------------------------------------------------------
      *> END-OF-MONTH - LAST CALENDAR DAY OF THE MONTH CONTAINING THE
      *> SUPPLIED DATE.  USED BY TERMCALC FOR "END OF MONTH PLUS N DAYS"
      *> AND FOR "DAY NN OF THE FOLLOWING MONTH" WHERE THE DAY DOES NOT
      *> EXIST IN THE TARGET MONTH (SPEC 01 SECTION 9).
      *>----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       FUNCTION-ID. END-OF-MONTH.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-LENGTHS.
           05  FILLER                  PIC 9(2) VALUE 31.
           05  FILLER                  PIC 9(2) VALUE 28.
           05  FILLER                  PIC 9(2) VALUE 31.
           05  FILLER                  PIC 9(2) VALUE 30.
           05  FILLER                  PIC 9(2) VALUE 31.
           05  FILLER                  PIC 9(2) VALUE 30.
           05  FILLER                  PIC 9(2) VALUE 31.
           05  FILLER                  PIC 9(2) VALUE 31.
           05  FILLER                  PIC 9(2) VALUE 30.
           05  FILLER                  PIC 9(2) VALUE 31.
           05  FILLER                  PIC 9(2) VALUE 30.
           05  FILLER                  PIC 9(2) VALUE 31.
       01  WS-LEN-TAB REDEFINES WS-LENGTHS.
           05  WS-LEN                  PIC 9(2) OCCURS 12 TIMES.
       01  WS-SPLIT.
           05  WS-Y                    PIC 9(4) VALUE 0.
           05  WS-M                    PIC 9(2) VALUE 0.
           05  WS-D                    PIC 9(2) VALUE 0.
       01  WS-LAST                     PIC 9(2) VALUE 0.
       LINKAGE SECTION.
       01  LK-DATE                     PIC 9(8).
       01  LK-RESULT                   PIC 9(8).
       PROCEDURE DIVISION USING LK-DATE RETURNING LK-RESULT.
       E-MAIN.
           MOVE 0 TO LK-RESULT
           MOVE LK-DATE TO WS-SPLIT
           IF WS-Y < 1601 OR WS-Y > 9999
               GOBACK
           END-IF
           IF WS-M < 1 OR WS-M > 12
               GOBACK
           END-IF
           MOVE WS-LEN(WS-M) TO WS-LAST
           IF WS-M = 2
               IF FUNCTION MOD(WS-Y, 400) = 0
                   MOVE 29 TO WS-LAST
               ELSE
                   IF FUNCTION MOD(WS-Y, 100) NOT = 0
                      AND FUNCTION MOD(WS-Y, 4) = 0
                       MOVE 29 TO WS-LAST
                   END-IF
               END-IF
           END-IF
           MOVE WS-LAST TO WS-D
           MOVE WS-SPLIT TO LK-RESULT
           GOBACK.
       END FUNCTION END-OF-MONTH.

      *>----------------------------------------------------------------
      *> WEEKDAY-OF - 1 MONDAY .. 7 SUNDAY, 0 WHEN THE DATE IS INVALID.
      *> DAY 1 OF THE COBOL INTEGER DATE (1601-01-01) WAS A MONDAY.
      *>----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       FUNCTION-ID. WEEKDAY-OF.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-INT                      PIC S9(9) VALUE 0.
       LINKAGE SECTION.
       01  LK-DATE                     PIC 9(8).
       01  LK-RESULT                   PIC S9(4).
       PROCEDURE DIVISION USING LK-DATE RETURNING LK-RESULT.
       W-MAIN.
           MOVE 0 TO LK-RESULT
           IF FUNCTION TEST-DATE-YYYYMMDD(LK-DATE) NOT = 0
               GOBACK
           END-IF
           COMPUTE WS-INT = FUNCTION INTEGER-OF-DATE(LK-DATE)
           COMPUTE LK-RESULT = FUNCTION MOD(WS-INT - 1, 7) + 1
           GOBACK.
       END FUNCTION WEEKDAY-OF.
