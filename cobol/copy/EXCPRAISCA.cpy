      *>================================================================
      *> EXCPRAISCA - COMMAREA for EXCPRAIS (exception worklist item)
      *> CA-FUNCTION value
      *>   RAISE     raise or refresh an exception item through
      *>             core.usp_RaiseException
      *> COM-017 - holds, warnings and hard errors must stay
      *> distinguishable, so EX-SEVERITY is one of HOLD, WARNING or
      *> ERROR and nothing else is accepted.
      *> COM-022 - an actionable exception has an owner, an age, a
      *> priority and a due date where relevant, and its resolution
      *> history is visible.  Raising the same category and document
      *> twice updates the open item rather than creating a second one.
      *> RETURN CODES 00 ok, 08 invalid input, 12 SQL failure.
      *>================================================================
      *> REQ: COM-017, COM-022
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  EX-BODY.
               10  EX-MODULE           PIC X(6).
               10  EX-SEVERITY         PIC X(12).
                   88  EX-HOLD                   VALUE "HOLD".
                   88  EX-WARNING                VALUE "WARNING".
                   88  EX-ERROR                  VALUE "ERROR".
               10  EX-CATEGORY         PIC X(40).
               10  EX-MESSAGE-TEXT     PIC X(200).
               10  EX-DOCUMENT-TYPE    PIC X(20).
               10  EX-DOCUMENT-REF     PIC X(40).
               10  EX-OWNER-USER       PIC X(20).
               10  EX-OWNER-ROLE       PIC X(20).
               10  EX-PRIORITY         PIC S9(4) SIGN LEADING SEPARATE.
               10  EX-DUE-AT           PIC 9(8).
               10  EX-AMOUNT-GBP       PIC S9(13)V9(6) COMP-3.
               10  EX-QUANTITY         PIC S9(13)V9(6) COMP-3.
               10  EX-EXCEPTION-ID     PIC S9(9) COMP-5.
               10  EX-STATUS           PIC X(12).
               10  EX-RAISED-AT        PIC X(23).
