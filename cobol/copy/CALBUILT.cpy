      *>================================================================
      *> CALBUILT - BUILT-IN UK BANK HOLIDAY TABLE, 2026 TO 2028
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : COMMON SERVICES
      *> PURPOSE  : FALLBACK CALENDAR USED BY DATECALC WHEN THE
      *>            core.Calendar / core.CalendarDay TABLES CANNOT BE
      *>            READ (DATABASE NOT DEPLOYED, OR THE SITE RUNS THE
      *>            ENGINE WITHOUT SQL).  THE DATABASE ALWAYS WINS WHEN
      *>            IT IS AVAILABLE - SEE DATECALC 7000-LOAD-CALENDAR.
      *>            LAYOUT PER ENTRY:
      *>              CALENDAR  X(4)   ENG / SCT / NIR
      *>              DATE      9(8)   YYYYMMDD
      *>              NAME      X(28)
      *>            THE BACS PROCESSING CALENDAR REUSES THE ENGLAND AND
      *>            WALES ENTRIES: BACS DOES NOT PROCESS ON AN ENGLAND
      *>            AND WALES BANK HOLIDAY OR AT A WEEKEND (COM-026).
      *>            Compiles in fixed and free format (cols 8-72, *>).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-01-08 DJM  CR0501 HARD CODED HOLIDAY TABLE (3 YEARS)
      *> 2026-10-05 STERP REFRESHED FOR 2026-2028, SUBSTITUTE DAYS
      *>================================================================
      *> REQ: COM-026   AC: AC-112
       01  CAL-BUILT-IN.
      *>   England and Wales 2026
           05  FILLER PIC X(40) VALUE "ENG20260101New Year's Day".
           05  FILLER PIC X(40) VALUE "ENG20260403Good Friday".
           05  FILLER PIC X(40) VALUE "ENG20260406Easter Monday".
           05  FILLER PIC X(40) VALUE "ENG20260504Early May".
           05  FILLER PIC X(40) VALUE "ENG20260525Spring".
           05  FILLER PIC X(40) VALUE "ENG20260831Summer".
           05  FILLER PIC X(40) VALUE "ENG20261225Christmas Day".
           05  FILLER PIC X(40)
                  VALUE "ENG20261228Boxing Day substitute".
      *>   England and Wales 2027 (25 Dec Sat, 26 Dec Sun)
           05  FILLER PIC X(40) VALUE "ENG20270101New Year's Day".
           05  FILLER PIC X(40) VALUE "ENG20270326Good Friday".
           05  FILLER PIC X(40) VALUE "ENG20270329Easter Monday".
           05  FILLER PIC X(40) VALUE "ENG20270503Early May".
           05  FILLER PIC X(40) VALUE "ENG20270531Spring".
           05  FILLER PIC X(40) VALUE "ENG20270830Summer".
           05  FILLER PIC X(40) VALUE "ENG20271227Christmas substitute".
           05  FILLER PIC X(40)
                  VALUE "ENG20271228Boxing Day substitute".
      *>   England and Wales 2028 (1 Jan Sat)
           05  FILLER PIC X(40) VALUE "ENG20280103New Year substitute".
           05  FILLER PIC X(40) VALUE "ENG20280414Good Friday".
           05  FILLER PIC X(40) VALUE "ENG20280417Easter Monday".
           05  FILLER PIC X(40) VALUE "ENG20280501Early May".
           05  FILLER PIC X(40) VALUE "ENG20280529Spring".
           05  FILLER PIC X(40) VALUE "ENG20280828Summer".
           05  FILLER PIC X(40) VALUE "ENG20281225Christmas Day".
           05  FILLER PIC X(40) VALUE "ENG20281226Boxing Day".
      *>   Scotland 2026 - 2 January, no Easter Monday, St Andrew's Day
           05  FILLER PIC X(40) VALUE "SCT20260101New Year's Day".
           05  FILLER PIC X(40) VALUE "SCT202601022 January".
           05  FILLER PIC X(40) VALUE "SCT20260403Good Friday".
           05  FILLER PIC X(40) VALUE "SCT20260504Early May".
           05  FILLER PIC X(40) VALUE "SCT20260803Summer".
           05  FILLER PIC X(40) VALUE "SCT20261130St Andrew's Day".
           05  FILLER PIC X(40) VALUE "SCT20261225Christmas Day".
           05  FILLER PIC X(40)
                  VALUE "SCT20261228Boxing Day substitute".
      *>   Scotland 2027 (2 Jan Sat - substitute Monday 4 January)
           05  FILLER PIC X(40) VALUE "SCT20270101New Year's Day".
           05  FILLER PIC X(40) VALUE "SCT202701042 January substitute".
           05  FILLER PIC X(40) VALUE "SCT20270326Good Friday".
           05  FILLER PIC X(40) VALUE "SCT20270503Early May".
           05  FILLER PIC X(40) VALUE "SCT20270802Summer".
           05  FILLER PIC X(40) VALUE "SCT20271130St Andrew's Day".
           05  FILLER PIC X(40) VALUE "SCT20271227Christmas substitute".
           05  FILLER PIC X(40)
                  VALUE "SCT20271228Boxing Day substitute".
      *>   Scotland 2028 (1 Jan Sat, 2 Jan Sun)
           05  FILLER PIC X(40) VALUE "SCT20280103New Year substitute".
           05  FILLER PIC X(40) VALUE "SCT202801042 January substitute".
           05  FILLER PIC X(40) VALUE "SCT20280414Good Friday".
           05  FILLER PIC X(40) VALUE "SCT20280501Early May".
           05  FILLER PIC X(40) VALUE "SCT20280807Summer".
           05  FILLER PIC X(40) VALUE "SCT20281130St Andrew's Day".
           05  FILLER PIC X(40) VALUE "SCT20281225Christmas Day".
           05  FILLER PIC X(40) VALUE "SCT20281226Boxing Day".
      *>   Northern Ireland 2026 - St Patrick's and the Twelfth
           05  FILLER PIC X(40) VALUE "NIR20260101New Year's Day".
           05  FILLER PIC X(40) VALUE "NIR20260317St Patrick's Day".
           05  FILLER PIC X(40) VALUE "NIR20260403Good Friday".
           05  FILLER PIC X(40) VALUE "NIR20260406Easter Monday".
           05  FILLER PIC X(40) VALUE "NIR20260504Early May".
           05  FILLER PIC X(40) VALUE "NIR20260525Spring".
           05  FILLER PIC X(40) VALUE "NIR20260713Boyne substitute".
           05  FILLER PIC X(40) VALUE "NIR20260831Summer".
           05  FILLER PIC X(40) VALUE "NIR20261225Christmas Day".
           05  FILLER PIC X(40)
                  VALUE "NIR20261228Boxing Day substitute".
      *>   Northern Ireland 2027
           05  FILLER PIC X(40) VALUE "NIR20270101New Year's Day".
           05  FILLER PIC X(40) VALUE "NIR20270317St Patrick's Day".
           05  FILLER PIC X(40) VALUE "NIR20270326Good Friday".
           05  FILLER PIC X(40) VALUE "NIR20270329Easter Monday".
           05  FILLER PIC X(40) VALUE "NIR20270503Early May".
           05  FILLER PIC X(40) VALUE "NIR20270531Spring".
           05  FILLER PIC X(40) VALUE "NIR20270712Battle of the Boyne".
           05  FILLER PIC X(40) VALUE "NIR20270830Summer".
           05  FILLER PIC X(40) VALUE "NIR20271227Christmas substitute".
           05  FILLER PIC X(40)
                  VALUE "NIR20271228Boxing Day substitute".
      *>   Northern Ireland 2028
           05  FILLER PIC X(40) VALUE "NIR20280103New Year substitute".
           05  FILLER PIC X(40) VALUE "NIR20280317St Patrick's Day".
           05  FILLER PIC X(40) VALUE "NIR20280414Good Friday".
           05  FILLER PIC X(40) VALUE "NIR20280417Easter Monday".
           05  FILLER PIC X(40) VALUE "NIR20280501Early May".
           05  FILLER PIC X(40) VALUE "NIR20280529Spring".
           05  FILLER PIC X(40) VALUE "NIR20280712Battle of the Boyne".
           05  FILLER PIC X(40) VALUE "NIR20280828Summer".
           05  FILLER PIC X(40) VALUE "NIR20281225Christmas Day".
           05  FILLER PIC X(40) VALUE "NIR20281226Boxing Day".
       01  CAL-BUILT-TAB REDEFINES CAL-BUILT-IN.
           05  CAL-ENTRY OCCURS 78 TIMES.
               10  CAL-E-CODE          PIC X(3).
               10  CAL-E-DATE          PIC 9(8).
               10  CAL-E-NAME          PIC X(28).
               10  FILLER              PIC X.
       01  CAL-BUILT-COUNT             PIC S9(4) VALUE 78.
