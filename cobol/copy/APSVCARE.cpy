      *>================================================================
      *> APSVCARE - PURCHASE LEDGER COMMON SERVICE REQUEST AREA
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE  : ONE REQUEST AREA FOR THE SITE SERVICE WRAPPER
      *>            APSVC, WHICH IS THE PURCHASE LEDGER'S SINGLE ROUTE
      *>            TO THE COMMON SERVICES (PERMCHK, PARMGET, AUDITLOG,
      *>            EXCPRAIS, NUMGEN, PERIODCK, TERMCALC, FXCONV,
      *>            DATECALC, STRUTIL, RNDALLOC, IDEMPOT).
      *>            Written in 1991 so that the ledger programs would
      *>            not each have to carry a copy of every service
      *>            linkage area; it is why no AP program holds more
      *>            than one COMMAREA.
      *>              CALL "APSVC" USING WS-APSVC
      *> FUNCTIONS: PERM PARMNUM PARMTXT AUDIT EXCEPT NUMBER PERIOD
      *>            TERMS FXTOGBP FXREAL NORMREF ROUND ADDDAYS DAYSBTWN
      *>            BUSDAY ADJUST ADDBUS IDEMBEG IDEMEND
      *>----------------------------------------------------------------
      *> 1991-02-28 RJH  ORIGINAL SERVICE WRAPPER
      *> 2026-10-05 STERP MAPPED ONTO THE STERP COMMON SERVICES
      *>================================================================
      *> REQ: COM-002, COM-005, COM-009, COM-011, COM-013, COM-017
       01  WS-APSVC.
           05  SVC-FUNC                PIC X(8).
           05  SVC-COMPANY             PIC X(10).
           05  SVC-SITE                PIC X(10).
           05  SVC-USER                PIC X(20).
           05  SVC-DATE                PIC 9(8).
           05  SVC-CORRELATION         PIC X(36).
           05  SVC-RC                  PIC 9(2).
               88  SVC-OK                        VALUE 00.
               88  SVC-WARN                      VALUE 04.
               88  SVC-HELD                      VALUE 06.
               88  SVC-BAD                       VALUE 08 THRU 16.
           05  SVC-REASON              PIC X(8).
           05  SVC-MESSAGE             PIC X(200).
           05  SVC-TEXT-1              PIC X(60).
           05  SVC-TEXT-2              PIC X(60).
           05  SVC-TEXT-3              PIC X(200).
           05  SVC-TEXT-OUT            PIC X(200).
           05  SVC-FLAG-1              PIC X.
           05  SVC-FLAG-2              PIC X.
           05  SVC-DATE-1              PIC 9(8).
           05  SVC-DATE-2              PIC 9(8).
           05  SVC-DATE-OUT            PIC 9(8).
           05  SVC-DATE-OUT2           PIC 9(8).
           05  SVC-NUM-1               PIC S9(13)V9(6).
           05  SVC-NUM-2               PIC S9(13)V9(6).
           05  SVC-NUM-3               PIC S9(13)V9(6).
           05  SVC-NUM-OUT             PIC S9(13)V9(6).
           05  SVC-NUM-OUT2            PIC S9(13)V9(6).
           05  SVC-INT-1               PIC S9(9).
           05  SVC-INT-OUT             PIC S9(9).
           05  SVC-INT-OUT2            PIC S9(9).
           05  SVC-AUD-ENTITY          PIC X(40).
           05  SVC-AUD-KEY             PIC X(60).
           05  SVC-AUD-ACTION          PIC X(20).
           05  SVC-AUD-FIELD           PIC X(40).
           05  SVC-AUD-OLD             PIC X(100).
           05  SVC-AUD-NEW             PIC X(100).
           05  SVC-AUD-REASON          PIC X(200).
           05  SVC-AUD-AUTH            PIC X(40).
           05  SVC-AUD-SENS            PIC X.
           05  SVC-EXC-SEVERITY        PIC X(8).
           05  SVC-EXC-CATEGORY        PIC X(30).
           05  SVC-EXC-DOCTYPE         PIC X(20).
           05  SVC-EXC-DOCREF          PIC X(40).
           05  SVC-EXC-OWNER           PIC X(20).
           05  SVC-EXC-ROLE            PIC X(20).
           05  SVC-EXC-MESSAGE         PIC X(200).
           05  SVC-EXC-AMOUNT          PIC S9(13)V99.
           05  SVC-EXC-ID              PIC S9(9).
