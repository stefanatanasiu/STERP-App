      *>================================================================
      *> CMDARGSCA - COMMAREA for CMDARGS (batch command line parser)
      *> CA-FUNCTION values
      *>   CMDLINE   read the command line of the running batch EXE and
      *>             parse it (batch programs only)
      *>   PARSE     parse the text already in CM-COMMAND-LINE
      *>   GET       return the value of CM-LOOKUP-KEY from the pairs
      *>             already parsed -> CM-LOOKUP-VALUE, CM-FOUND
      *> Arguments are KEY=VALUE pairs separated by spaces, as used by
      *> the job streams, for example
      *>     AGEDDEBT CO=STC ASAT=20261004 USER=ARCLERK1 MODE=PREVIEW
      *> Keys are folded to upper case.  A value may be quoted with
      *> double quotes when it contains spaces.  The first token is
      *> ignored when it has no "=" in it (some job streams repeat the
      *> program name).  The common keys are also copied into their own
      *> fields so that callers do not have to search the table.
      *> RETURN CODES 00 ok, 04 some pairs were discarded (table full),
      *>   08 malformed argument (CMD0001..CMD0004), 16 key not found.
      *>================================================================
      *> REQ: COM-018
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  CM-BODY.
               10  CM-COMMAND-LINE     PIC X(256).
               10  CM-PAIR-CNT         PIC S9(9) COMP-5.
               10  CM-DROPPED          PIC S9(9) COMP-5.
               10  CM-LOOKUP-KEY       PIC X(12).
               10  CM-LOOKUP-VALUE     PIC X(60).
               10  CM-FOUND            PIC X.
               10  CM-CO               PIC X(10).
               10  CM-SITE             PIC X(10).
               10  CM-USER             PIC X(20).
               10  CM-ASAT             PIC X(8).
               10  CM-FROM             PIC X(8).
               10  CM-TO               PIC X(8).
               10  CM-PERIOD           PIC X(6).
               10  CM-MODE             PIC X(10).
               10  CM-OUT              PIC X(60).
               10  CM-PAIR OCCURS 20 TIMES.
                   15  CM-KEY          PIC X(12).
                   15  CM-VALUE        PIC X(60).
