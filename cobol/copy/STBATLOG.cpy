      *>================================================================
      *> STBATLOG - STERP standard batch log / console message area
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : COMMON SERVICES
      *> PURPOSE  : Console message ids and counters shared by every
      *>            batch main program.  Messages follow the operator
      *>            convention  STERP-I-nnnn / STERP-W-nnnn /
      *>            STERP-E-nnnn  and the final spool line is
      *>            STERP-I-SPOOL <full path>.
      *>            Usage:
      *>              MOVE "STERP-I-0001" TO BL-MSG-ID
      *>              MOVE "Run started"  TO BL-MSG-TEXT
      *>              DISPLAY BL-MSG-ID " " FUNCTION TRIM(BL-MSG-TEXT)
      *>            Keep BL-RETURN-CODE in step with the online
      *>            convention: 0 / 4 / 8 / 12 / 16.
      *>            Compiles in fixed and free format (cols 8-72, *>).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1993-03-02 DJM  CR0151 OPERATOR MESSAGE STANDARD
      *> 2004-08-30 KMP  JOB RUN REGISTRATION (NIGHT SHIFT MONITOR)
      *> 2026-10-05 STERP core.JobRun VIA JOBLOG SERVICE
      *>================================================================
      *> REQ: COM-014, COM-018
       01  BL-BATCH-LOG.
           05  BL-JOB-NAME             PIC X(30) VALUE SPACES.
           05  BL-MSG-ID               PIC X(12) VALUE SPACES.
           05  BL-MSG-TEXT             PIC X(120) VALUE SPACES.
           05  BL-RETURN-CODE          PIC S9(4) VALUE 0.
               88  BL-RC-OK                      VALUE 0.
               88  BL-RC-WARNING                 VALUE 4.
               88  BL-RC-ERROR                   VALUE 8.
               88  BL-RC-SEVERE                  VALUE 12.
               88  BL-RC-NOT-FOUND               VALUE 16.
           05  BL-READ-COUNT           PIC S9(9) VALUE 0.
           05  BL-SELECTED-COUNT       PIC S9(9) VALUE 0.
           05  BL-WRITTEN-COUNT        PIC S9(9) VALUE 0.
           05  BL-REJECTED-COUNT       PIC S9(9) VALUE 0.
           05  BL-WARNING-COUNT        PIC S9(9) VALUE 0.
           05  BL-ERROR-COUNT          PIC S9(9) VALUE 0.
           05  BL-COUNT-ED             PIC Z(8)9.
           05  BL-SPOOL-FILE           PIC X(200) VALUE SPACES.
           05  BL-LOG-FILE             PIC X(200) VALUE SPACES.
           05  BL-MODE                 PIC X(10) VALUE "PREVIEW".
               88  BL-PREVIEW                    VALUE "PREVIEW".
               88  BL-COMMIT                     VALUE "COMMIT".
