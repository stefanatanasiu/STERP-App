      *>================================================================
      *> IDEMPOTCA - COMMAREA for IDEMPOT (repeated instructions)
      *> CA-FUNCTION values
      *>   BEGIN     claim CA-CORRELATION-ID for ID-OPERATION through
      *>             core.usp_IdempotentBegin.  ID-OUTCOME says
      *>             STARTED (this caller owns the instruction),
      *>             INPROGRESS (another attempt is running - the
      *>             outcome is not yet known) or COMPLETED / FAILED
      *>             (the instruction already finished; ID-RESULT-REF
      *>             identifies the original result).
      *>   END       finish it with ID-STATUS COMPLETED or FAILED
      *>             through core.usp_IdempotentEnd
      *> COM-015 - repeating the same confirmed instruction must not
      *> create a second financial, stock or settlement effect, and the
      *> user must be able to tell an existing success from a new
      *> action and from an unresolved outcome - hence the distinct
      *> INPROGRESS answer rather than a plain "already done".
      *> RETURN CODES 00 proceed (STARTED), 04 already completed, 06
      *>   another attempt is in progress, 08 invalid input or the
      *>   earlier attempt failed, 12 SQL failure.
      *>================================================================
      *> REQ: COM-015, COM-014
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  ID-BODY.
               10  ID-OPERATION        PIC X(60).
               10  ID-STATUS           PIC X(12).
               10  ID-OUTCOME          PIC X(12).
                   88  ID-STARTED                VALUE "STARTED".
                   88  ID-INPROGRESS             VALUE "INPROGRESS".
                   88  ID-COMPLETED              VALUE "COMPLETED".
                   88  ID-FAILED                 VALUE "FAILED".
               10  ID-RESULT-REF       PIC X(60).
               10  ID-RESULT-MESSAGE   PIC X(200).
               10  ID-STARTED-AT       PIC X(23).
               10  ID-ENDED-AT         PIC X(23).
