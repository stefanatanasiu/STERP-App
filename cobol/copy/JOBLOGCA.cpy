      *>================================================================
      *> JOBLOGCA - COMMAREA for JOBLOG (batch job run registration)
      *> CA-FUNCTION values
      *>   START     register a run in core.JobRun through
      *>             core.usp_JobRunStart -> JL-JOB-RUN-ID
      *>   END       close it through core.usp_JobRunEnd with
      *>             JL-STATUS, JL-RETURN-CODE, JL-ROWS-PROCESSED and
      *>             the spool and log file names
      *> Every batch main program registers itself so that the job
      *> monitor in the client can show what ran, how it ended and
      *> which spool file it produced.  JL-STATUS is RUNNING, COMPLETE,
      *> WARNING, FAILED or CANCELLED; JL-RETURN-CODE follows the
      *> 0/4/8/12/16 convention.
      *> RETURN CODES 00 ok, 08 invalid input, 12 SQL failure.
      *>================================================================
      *> REQ: COM-014, COM-018
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  JL-BODY.
               10  JL-JOB-NAME         PIC X(40).
               10  JL-JOB-RUN-ID       PIC S9(9) COMP-5.
               10  JL-STATUS           PIC X(12).
               10  JL-RETURN-CODE      PIC S9(4) SIGN LEADING SEPARATE.
               10  JL-ROWS-PROCESSED   PIC S9(9) COMP-5.
               10  JL-ARGUMENTS        PIC X(200).
               10  JL-LOG-FILE         PIC X(200).
               10  JL-SPOOL-FILE       PIC X(200).
               10  JL-MESSAGE-TEXT     PIC X(200).
               10  JL-STARTED-AT       PIC X(23).
               10  JL-ENDED-AT         PIC X(23).
