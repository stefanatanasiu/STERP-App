      *>================================================================
      *> PERMCHKCA - COMMAREA for PERMCHK (authority check, COM-002)
      *> CA-FUNCTION value
      *>   CHECK     ask core.usp_CheckPermission whether PK-CHECK-USER
      *>             (default CA-USER) holds PK-PERMISSION for
      *>             CA-COMPANY and PK-CHECK-SITE (default CA-SITE)
      *> COM-002 - every business action and sensitive enquiry or
      *> export needs authority for the action, the company and the
      *> applicable site.  An unauthorised attempt has no business
      *> effect and is explained, so a refusal is return code 06
      *> (needs authority) with the reason text from the procedure -
      *> never a silent failure.
      *> RETURN CODES 00 allowed, 06 not allowed (PK-ALLOWED = "N"),
      *>   08 invalid input, 12 SQL failure.
      *>================================================================
      *> REQ: COM-002, COM-019
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  PK-BODY.
               10  PK-PERMISSION       PIC X(40).
               10  PK-CHECK-USER       PIC X(20).
               10  PK-CHECK-SITE       PIC X(10).
               10  PK-ALLOWED          PIC X.
               10  PK-REASON-TEXT      PIC X(200).
