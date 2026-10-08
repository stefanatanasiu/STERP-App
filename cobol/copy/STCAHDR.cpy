      *>================================================================
      *> STCAHDR  - STERP standard COMMAREA header (400 bytes)
      *>           COPY at level 05 as the first item of every COMMAREA
      *>           e.g.   01  DFHCOMMAREA.
      *>                      COPY STCAHDR.
      *>                      05  XX-BODY. ...
      *> RETURN CODES follow the old CICS/batch convention:
      *>   00 OK, 04 WARNING (proceed), 06 HOLD (needs authority),
      *>   08 ERROR (no business effect), 12 SEVERE, 16 NOT FOUND
      *> 1991-02-04 original (CICS TS 1.x)   2011-08-08 .NET client
      *>================================================================
           05  CA-HEADER.
               10  CA-PROGRAM          PIC X(8).
               10  CA-FUNCTION         PIC X(8).
               10  CA-COMPANY          PIC X(10).
               10  CA-SITE             PIC X(10).
               10  CA-USER             PIC X(20).
               10  CA-BUSINESS-DATE    PIC 9(8).
               10  CA-CORRELATION-ID   PIC X(36).
               10  CA-RETURN-CODE      PIC 9(2).
                   88  CA-OK                     VALUE 00.
                   88  CA-WARNING                VALUE 04.
                   88  CA-HOLD                   VALUE 06.
                   88  CA-ERROR                  VALUE 08.
                   88  CA-SEVERE                 VALUE 12.
                   88  CA-NOT-FOUND              VALUE 16.
                   88  CA-FAILED                 VALUE 08 THRU 16.
               10  CA-REASON-CODE      PIC X(8).
               10  CA-MESSAGE          PIC X(200).
               10  CA-REQ-REF          PIC X(10).
               10  CA-MSG-COUNT        PIC 9(2).
               10  FILLER              PIC X(78).
