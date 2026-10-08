      *>================================================================
      *> APXMLGTC - COMMAREA for APXMLGET (one line of a structured
      *>            e-invoice taken apart).
      *> FUNCTIONS  : ELEMENT  first element on the record
      *>              FIND     a named element anywhere on the record
      *>              DATE     XG-VALUE (yyyy-mm-dd) to a date number
      *>              NUM      XG-VALUE to a signed number
      *> 2026-10-06 STERP AP-031 ORIGINAL
      *>================================================================
      *> REQ: AP-031
       01  DFHCOMMAREA.
           05  XG-FUNC                 PIC X(8).
               88  XG-F-ELEMENT                  VALUE "ELEMENT".
               88  XG-F-FIND                     VALUE "FIND".
               88  XG-F-DATE                     VALUE "DATE".
               88  XG-F-NUM                      VALUE "NUM".
           05  XG-RC                   PIC 9(2).
               88  XG-OK                         VALUE 00.
               88  XG-NO-ELEMENT                 VALUE 04.
               88  XG-BAD                        VALUE 08.
           05  XG-REASON               PIC X(8).
           05  XG-RECORD               PIC X(300).
           05  XG-TAG-WANTED           PIC X(40).
           05  XG-TAG-FOUND            PIC X(40).
           05  XG-VALUE                PIC X(200).
           05  XG-VALUE-LEN            PIC S9(4).
           05  XG-ATTRIBUTES           PIC X(100).
           05  XG-IS-OPEN              PIC X.
               88  XG-CONTAINER-OPEN             VALUE "Y".
           05  XG-IS-CLOSE             PIC X.
               88  XG-CONTAINER-CLOSE            VALUE "Y".
           05  XG-IS-EMPTY             PIC X.
           05  XG-NUMERIC              PIC S9(13)V9(6).
           05  XG-DATE                 PIC 9(8).
