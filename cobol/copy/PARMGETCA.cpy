      *>================================================================
      *> PARMGETCA - COMMAREA for PARMGET (effective dated parameter)
      *> CA-FUNCTION values
      *>   GET       read the parameter effective on PM-AS-OF for
      *>             CA-COMPANY, falling back to the all company row
      *>             ("*").  Calls core.usp_GetParameter.
      *>   GETNUM    as GET, and insist that the value is numeric
      *> PM-AS-OF defaults to CA-BUSINESS-DATE and then to today.
      *> PM-VALUE-NUM is filled whenever the value looks numeric.
      *> When the parameter is not held and PM-USE-DEFAULT is "Y" the
      *> caller's PM-DEFAULT-TEXT is returned with return code 04, so
      *> that a missing tuning parameter cannot stop a business action;
      *> otherwise the answer is 16 (not found).
      *> RETURN CODES 00 ok, 04 default used, 08 invalid input or a
      *>   non-numeric value for GETNUM, 12 SQL failure, 16 not held.
      *>================================================================
      *> REQ: COM-011, COM-025
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  PM-BODY.
               10  PM-PARAM-CODE       PIC X(40).
               10  PM-AS-OF            PIC 9(8).
               10  PM-VALUE-TEXT       PIC X(200).
               10  PM-VALUE-NUM        PIC S9(13)V9(6) COMP-3.
               10  PM-DATA-TYPE        PIC X(12).
               10  PM-UNIT             PIC X(20).
               10  PM-DECISION-REF     PIC X(10).
               10  PM-OWNER-ROLE       PIC X(20).
               10  PM-DESCRIPTION      PIC X(100).
               10  PM-EFFECTIVE-FROM   PIC 9(8).
               10  PM-SCOPE            PIC X(10).
               10  PM-DEFAULT-TEXT     PIC X(200).
               10  PM-USE-DEFAULT      PIC X.
               10  PM-FOUND            PIC X.
               10  PM-IS-NUMERIC       PIC X.
