      *>================================================================
      *> RUNPATHCA - COMMAREA for RUNPATH (batch output path builder)
      *> CA-FUNCTION values
      *>   BASE      return the run directory only -> RP-BASE-PATH
      *>   BUILD     build RP-FULL-PATH for an output file
      *> The run directory comes from the environment variable
      *> STERP_RUN; when that is not set, "run" relative to the current
      *> directory is used and RP-SOURCE says DEFAULT.
      *> RP-FOLDER selects the sub directory and its default extension
      *>     SPOOL      run\spool      .lst   132 column reports
      *>     BACS       run\bacs       .txt   Bacs payment files
      *>     MTD        run\mtd        .json  Making Tax Digital
      *>     EINVOICE   run\einvoice   .xml   structured invoices
      *>     EXTRACTS   run\extracts   .csv   authorised exports
      *>     LOGS       run\logs       .log   job logs
      *>     WORK       run\work       .tmp   scratch files
      *> The file name is  <RP-REPORT-ID>-<RP-COMPANY-REF>-<stamp>.<ext>
      *> with the stamp (yyyymmddhhmmss) omitted when RP-USE-TIMESTAMP
      *> is "N".  RP-TIMESTAMP is filled in from the system clock unless
      *> the caller supplies one, which keeps a job's files together.
      *> RETURN CODES 00 ok, 08 invalid input (RUN0001..RUN0004).
      *>================================================================
      *> REQ: COM-018, COM-020
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  RP-BODY.
               10  RP-FOLDER           PIC X(10).
               10  RP-REPORT-ID        PIC X(12).
               10  RP-COMPANY-REF      PIC X(10).
               10  RP-EXTENSION        PIC X(8).
               10  RP-TIMESTAMP        PIC X(14).
               10  RP-USE-TIMESTAMP    PIC X.
               10  RP-FILE-NAME        PIC X(100).
               10  RP-FULL-PATH        PIC X(200).
               10  RP-BASE-PATH        PIC X(160).
               10  RP-SOURCE           PIC X(8).
                   88  RP-FROM-ENV               VALUE "ENV".
                   88  RP-FROM-DEFAULT           VALUE "DEFAULT".
