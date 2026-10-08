      *>================================================================
      *> STRPTHDR - STERP standard 132 column report page heading
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : COMMON SERVICES
      *> PURPOSE  : REPORT SECTION half of the page heading convention.
      *>            COPY inside an RD, after the PAGE LIMIT clause:
      *>
      *>              REPORT SECTION.
      *>              RD  R-AGEDDEBT
      *>                  CONTROLS ARE FINAL
      *>                  PAGE LIMIT 60 LINES
      *>                  HEADING 1 FIRST DETAIL 7 LAST DETAIL 58
      *>                  FOOTING 60.
      *>              COPY STRPTHDR.
      *>              01  DE-LINE TYPE DETAIL. ...
      *>
      *>            Needs COPY STRPTWS. in WORKING-STORAGE.  Reserve
      *>            FIRST DETAIL 7 - the heading occupies lines 1 to 6.
      *>            Layout (132 columns):
      *>              1  company name .......... report id .... page n
      *>              2  registered number / place of registration
      *>              3  report title ................. run dd/mm/yyyy
      *>              4  subtitle / as at text ............ user, time
      *>              5  blank
      *>              6  rule
      *>            Compiles in fixed and free format (cols 8-72, *>).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1993-03-02 DJM  CR0151 STANDARD HEADINGS FOR THE BATCH SUITE
      *> 1997-05-19 RJH  CR0702 PAGE NUMBER MOVED RIGHT (PRINTER 3812)
      *> 2026-10-05 STERP REGISTERED NUMBER LINE (COM-020)
      *>================================================================
      *> REQ: COM-020, COM-024
       01  PH-PAGE-HEAD TYPE IS PAGE HEADING.
           05  LINE 1.
               10  COLUMN 1    PIC X(40) SOURCE RPT-COMPANY-NAME.
               10  COLUMN 50   PIC X(12) SOURCE RPT-REPORT-ID.
               10  COLUMN 112  PIC X(5)  VALUE "Page ".
               10  COLUMN 117  PIC ZZZ9  SOURCE PAGE-COUNTER.
           05  LINE 2.
               10  COLUMN 1    PIC X(19)
                               VALUE "Registered number: ".
               10  COLUMN 20   PIC X(10) SOURCE RPT-REG-NUMBER.
               10  COLUMN 32   PIC X(25)
                               VALUE "Place of registration: ".
               10  COLUMN 57   PIC X(24) SOURCE RPT-REG-PLACE.
           05  LINE 3.
               10  COLUMN 1    PIC X(60) SOURCE RPT-TITLE.
               10  COLUMN 104  PIC X(9)  VALUE "Run date ".
               10  COLUMN 113  PIC X(10) SOURCE RPT-RUN-DATE.
           05  LINE 4.
               10  COLUMN 1    PIC X(60) SOURCE RPT-SUBTITLE.
               10  COLUMN 70   PIC X(30) SOURCE RPT-AS-AT-TEXT.
               10  COLUMN 104  PIC X(20) SOURCE RPT-USER.
               10  COLUMN 125  PIC X(8)  SOURCE RPT-RUN-TIME.
           05  LINE 6.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
