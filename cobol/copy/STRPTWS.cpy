      *>================================================================
      *> STRPTWS  - STERP standard report / batch heading work area
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : COMMON SERVICES
      *> PURPOSE  : WORKING-STORAGE half of the 132 column report page
      *>            heading convention.  COPY STRPTWS. in
      *>            WORKING-STORAGE and COPY STRPTHDR. inside the RD of
      *>            the REPORT SECTION - the RD refers to these names.
      *>            Fill RPT-COMPANY-NAME, RPT-REPORT-ID, RPT-TITLE and
      *>            RPT-RUN-* before the first GENERATE.
      *>            Compiles in fixed and free format (cols 8-72, *>).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1993-03-02 DJM  CR0151 STANDARD HEADINGS FOR THE BATCH SUITE
      *> 1999-11-30 KMP  Y2K - RUN DATE AS DD/MM/YYYY
      *> 2026-10-05 STERP STATUTORY TRADING DISCLOSURES (COM-020)
      *>================================================================
      *> REQ: COM-020, COM-024
       01  RPT-HEAD-WORK.
           05  RPT-COMPANY-CODE        PIC X(10) VALUE SPACES.
           05  RPT-COMPANY-NAME        PIC X(40) VALUE SPACES.
           05  RPT-REG-NUMBER          PIC X(10) VALUE SPACES.
           05  RPT-REG-PLACE           PIC X(24)
                                       VALUE "England and Wales".
           05  RPT-REPORT-ID           PIC X(12) VALUE SPACES.
           05  RPT-TITLE               PIC X(60) VALUE SPACES.
           05  RPT-SUBTITLE            PIC X(60) VALUE SPACES.
           05  RPT-RUN-DATE.
               10  RPT-RUN-DD          PIC 9(2) VALUE 0.
               10  FILLER              PIC X VALUE "/".
               10  RPT-RUN-MM          PIC 9(2) VALUE 0.
               10  FILLER              PIC X VALUE "/".
               10  RPT-RUN-YYYY        PIC 9(4) VALUE 0.
           05  RPT-RUN-TIME.
               10  RPT-RUN-HH          PIC 9(2) VALUE 0.
               10  FILLER              PIC X VALUE ":".
               10  RPT-RUN-MI          PIC 9(2) VALUE 0.
               10  FILLER              PIC X VALUE ":".
               10  RPT-RUN-SS          PIC 9(2) VALUE 0.
           05  RPT-AS-AT-TEXT          PIC X(30) VALUE SPACES.
           05  RPT-USER                PIC X(20) VALUE SPACES.
           05  RPT-STATUS-TEXT         PIC X(20) VALUE SPACES.
           05  RPT-PAGE-WIDTH          PIC S9(4) VALUE 132.
