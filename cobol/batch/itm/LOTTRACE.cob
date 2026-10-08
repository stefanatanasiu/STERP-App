      *>================================================================
      *> PROGRAM-ID : LOTTRACE
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE (STOCK CONTROL)
      *> PURPOSE    : LOT AND SERIAL TRACEABILITY REPORT.  TAKES ONE
      *>              LOT REFERENCE OR ONE SERIAL NUMBER AND PRINTS ITS
      *>              GENEALOGY: BACKWARD TO THE RECEIPTS THAT BROUGHT
      *>              IT IN AND FORWARD TO THE ISSUES, DESPATCHES AND
      *>              SCRAP THAT TOOK IT OUT, TOGETHER WITH EVERY
      *>              QUALITY HOLD AND RELEASE AND THE SHELF LIFE
      *>              POSITION AT THE AS AT DATE.  THE TRACE IS READ
      *>              THROUGH LOTSER SO THAT THE REPORT AND THE ONLINE
      *>              ENQUIRY CAN NEVER DISAGREE.
      *> RUN AS     : LOTTRACE CO=STC ITEM=xxx LOT=yyy USER=QA1
      *>              [SERIAL=zzz] [ASAT=yyyymmdd]
      *>              [MODE=BOTH|FORWARD|BACKWARD]
      *> OUTPUT     : run\spool\LOTTRACE-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, LOTSER, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-09-30 DJM  CR0462 ORIGINAL - LOT TRACEABILITY LISTING
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS ON EVERY MOVEMENT DATE
      *> 2000-05-08 KMP  CR1003 SERIAL GENEALOGY FOR WARRANTY CLAIMS
      *> 2012-02-20 ANW  CR2740 REMAINING SHELF LIFE BANNER
      *> 2026-10-05 STERP ITM-011, ITM-016 HOLD EVIDENCE IN THE TRACE
      *>================================================================
      *> REQ: ITM-011, ITM-016
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOTTRACE.
       AUTHOR. D J MERRITT.
       DATE-WRITTEN. 30/09/1996.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO WS-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-LOTTRCE.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ITMWORK.
       COPY STRPTWS.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY LOTSERCA REPLACING ==DFHCOMMAREA== BY ==WS-LS==.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-SITE                 PIC X(10) VALUE SPACES.
           05  WS-ITEM                 PIC X(40) VALUE SPACES.
           05  WS-LOT                  PIC X(30) VALUE SPACES.
           05  WS-SERIAL               PIC X(40) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-ASAT                 PIC 9(8) VALUE 0.
           05  WS-MODE                 PIC X(10) VALUE SPACES.
       01  WS-RUN.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-TX                   PIC S9(4) VALUE 0.
           05  WS-SPOOL-OPEN           PIC X VALUE "N".
       01  WS-DET.
           05  L-SEQ                   PIC 9(3) VALUE 0.
           05  L-DIRECTION             PIC X(8) VALUE SPACES.
           05  L-TYPE                  PIC X(14) VALUE SPACES.
           05  L-DATE                  PIC X(10) VALUE SPACES.
           05  L-QTY                   PIC S9(11)V9(3) VALUE 0.
           05  L-SITE                  PIC X(10) VALUE SPACES.
           05  L-LOCATION              PIC X(18) VALUE SPACES.
           05  L-DOC-TYPE              PIC X(16) VALUE SPACES.
           05  L-DOC-REF               PIC X(24) VALUE SPACES.
           05  L-WHO                   PIC X(14) VALUE SPACES.
       01  WS-BANNER.
           05  B-IDENTITY              PIC X(44) VALUE SPACES.
           05  B-ITEM                  PIC X(40) VALUE SPACES.
           05  B-DESC                  PIC X(40) VALUE SPACES.
           05  B-EXPIRY                PIC X(10) VALUE SPACES.
           05  B-DAYS                  PIC S9(7) VALUE 0.
           05  B-STATUS                PIC X(30) VALUE SPACES.
           05  B-SUPPLIER              PIC X(20) VALUE SPACES.
           05  B-SUP-LOT               PIC X(40) VALUE SPACES.
           05  B-CERT                  PIC X(40) VALUE SPACES.
       01  WS-TOT.
           05  T-IN                    PIC S9(13)V9(3) VALUE 0.
           05  T-OUT                   PIC S9(13)V9(3) VALUE 0.
           05  T-HOLDS                 PIC S9(5) VALUE 0.
           05  T-MOVES                 PIC S9(5) VALUE 0.
       REPORT SECTION.
       RD  R-LOTTRCE
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 15 LAST DETAIL 56
           FOOTING 58.
       COPY STRPTHDR.
       01  RH-BANNER TYPE IS REPORT HEADING.
           05  LINE 8.
               10  COLUMN 1    PIC X(10) VALUE "Identity: ".
               10  COLUMN 11   PIC X(44) SOURCE B-IDENTITY.
               10  COLUMN 58   PIC X(6) VALUE "Item: ".
               10  COLUMN 64   PIC X(40) SOURCE B-ITEM.
           05  LINE 9.
               10  COLUMN 1    PIC X(13) VALUE "Description: ".
               10  COLUMN 14   PIC X(40) SOURCE B-DESC.
               10  COLUMN 58   PIC X(9) VALUE "Supplier ".
               10  COLUMN 67   PIC X(20) SOURCE B-SUPPLIER.
               10  COLUMN 90   PIC X(9) VALUE "Sup lot: ".
               10  COLUMN 99   PIC X(34) SOURCE B-SUP-LOT.
           05  LINE 10.
               10  COLUMN 1    PIC X(8) VALUE "Expiry: ".
               10  COLUMN 10   PIC X(10) SOURCE B-EXPIRY.
               10  COLUMN 23   PIC X(16) VALUE "Days remaining: ".
               10  COLUMN 39   PIC -(6)9 SOURCE B-DAYS.
               10  COLUMN 50   PIC X(8) VALUE "Status: ".
               10  COLUMN 58   PIC X(30) SOURCE B-STATUS.
           05  LINE 11.
               10  COLUMN 1    PIC X(13) VALUE "Certificate: ".
               10  COLUMN 14   PIC X(40) SOURCE B-CERT.
           05  LINE 13.
               10  COLUMN 1    PIC X(3) VALUE "Seq".
               10  COLUMN 5    PIC X(9) VALUE "Direction".
               10  COLUMN 15   PIC X(9) VALUE "Movement ".
               10  COLUMN 30   PIC X(4) VALUE "Date".
               10  COLUMN 45   PIC X(8) VALUE "Quantity".
               10  COLUMN 60   PIC X(4) VALUE "Site".
               10  COLUMN 71   PIC X(8) VALUE "Location".
               10  COLUMN 90   PIC X(8) VALUE "Document".
               10  COLUMN 107  PIC X(9) VALUE "Reference".
               10  COLUMN 121  PIC X(6) VALUE "Actor ".
           05  LINE 14.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC ZZ9 SOURCE L-SEQ.
               10  COLUMN 5    PIC X(8) SOURCE L-DIRECTION.
               10  COLUMN 15   PIC X(14) SOURCE L-TYPE.
               10  COLUMN 30   PIC X(10) SOURCE L-DATE.
               10  COLUMN 42   PIC Z(10)9.999- SOURCE L-QTY.
               10  COLUMN 60   PIC X(10) SOURCE L-SITE.
               10  COLUMN 71   PIC X(18) SOURCE L-LOCATION.
               10  COLUMN 90   PIC X(16) SOURCE L-DOC-TYPE.
               10  COLUMN 107  PIC X(13) SOURCE L-DOC-REF.
               10  COLUMN 121  PIC X(12) SOURCE L-WHO.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(18) VALUE "Traced movements:".
               10  COLUMN 20   PIC Z(4)9 SOURCE T-MOVES.
               10  COLUMN 30   PIC X(14) VALUE "Quantity in:".
               10  COLUMN 44   PIC Z(12)9.999 SOURCE T-IN.
               10  COLUMN 62   PIC X(15) VALUE "Quantity out:".
               10  COLUMN 77   PIC Z(12)9.999 SOURCE T-OUT.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(18) VALUE "Hold actions:".
               10  COLUMN 20   PIC Z(4)9 SOURCE T-HOLDS.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(46) VALUE
                   "Backward lines brought the identity into stock".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(46) VALUE
                   "and forward lines consumed or despatched it.".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(46) VALUE
                   "A hold changes availability, deleting nothing.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-READ-ARGS
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0200-JOB-START
           PERFORM 1000-READ-TRACE
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0300-OPEN-SPOOL
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 1200-PRINT-TRACE
           PERFORM 0400-CLOSE-SPOOL.
       0000-FINISH.
           PERFORM 0500-JOB-END
           MOVE WS-RC TO RETURN-CODE
           STOP RUN.

      *>----------------------------------------------------------------
       0100-READ-ARGS SECTION.
       0100-START.
           INITIALIZE WS-CMDARGS
           MOVE "CMDLINE" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           MOVE CM-CO TO WS-CO
           MOVE CM-SITE TO WS-SITE
           MOVE CM-USER TO WS-USER
           MOVE CM-MODE TO WS-MODE
           MOVE "ITEM" TO CM-LOOKUP-KEY
           PERFORM 0150-LOOKUP
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-ITEM
           END-IF
           MOVE "LOT" TO CM-LOOKUP-KEY
           PERFORM 0150-LOOKUP
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-LOT
           END-IF
           MOVE "SERIAL" TO CM-LOOKUP-KEY
           PERFORM 0150-LOOKUP
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-SERIAL
           END-IF
           IF CM-ASAT NOT = SPACES
               COMPUTE WS-ASAT = FUNCTION NUMVAL(CM-ASAT)
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "QA1" TO WS-USER
           END-IF
           IF WS-MODE = SPACES
               MOVE "BOTH" TO WS-MODE
           END-IF
           IF WS-ASAT = 0
               MOVE FUNCTION CURRENT-DATE TO WS-NOW
               COMPUTE WS-ASAT = FUNCTION NUMVAL(WS-NOW(1:8))
           END-IF
           IF WS-LOT = SPACES AND WS-SERIAL = SPACES
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0101 LOT= or SERIAL= is required"
               GO TO 0100-EXIT
           END-IF
           IF WS-ITEM = SPACES
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0102 ITEM= is required"
               GO TO 0100-EXIT
           END-IF
           DISPLAY "STERP-I-0100 LOTTRACE genealogy for "
                   FUNCTION TRIM(WS-ITEM).
       0100-EXIT.
           EXIT.

       0150-LOOKUP SECTION.
       0150-START.
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS.
       0150-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "LOTTRACE" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  ASK LOTSER FOR THE SHELF LIFE POSITION AND THE TRACE.
      *>       CR2740: THE BANNER STATES THE REMAINING LIFE BECAUSE A
      *>       LOT INSIDE ITS MINIMUM LIFE IS NOT THE SAME THING AS AN
      *>       EXPIRED LOT AND THE DIFFERENCE MATTERS TO THE RECALL.
      *>----------------------------------------------------------------
       1000-READ-TRACE SECTION.
       1000-START.
           PERFORM 1050-RESET-LS
           MOVE "EXPCHK" TO CA-FUNCTION OF WS-LS
           CALL "LOTSER" USING WS-LS
           MOVE SPACES TO B-STATUS
           EVALUATE TRUE
               WHEN CA-RETURN-CODE OF WS-LS = 0
                   MOVE "Usable at the as at date" TO B-STATUS
               WHEN CA-REASON-CODE OF WS-LS = "LOT0008"
                   MOVE "EXPIRED" TO B-STATUS
                   MOVE 4 TO WS-RC
               WHEN CA-REASON-CODE OF WS-LS = "LOT0009"
                   MOVE "Inside minimum shelf life" TO B-STATUS
                   MOVE 4 TO WS-RC
               WHEN OTHER
                   MOVE CA-MESSAGE OF WS-LS TO B-STATUS
           END-EVALUATE
           MOVE LS-DAYS-REMAINING TO B-DAYS
           MOVE SPACES TO B-EXPIRY
           IF LS-EXPIRY-DATE > 0
               MOVE LS-EXPIRY-DATE TO WS-DATE-N
               STRING WS-DATE-DD "/" WS-DATE-MM "/" WS-DATE-YYYY
                   DELIMITED BY SIZE INTO B-EXPIRY
           ELSE
               MOVE "none held" TO B-EXPIRY
           END-IF
           MOVE LS-SUPPLIER-CODE TO B-SUPPLIER
           MOVE LS-SUPPLIER-LOT TO B-SUP-LOT
           MOVE LS-CERTIFICATE-REF TO B-CERT
           PERFORM 1050-RESET-LS
           MOVE "TRACE" TO CA-FUNCTION OF WS-LS
           CALL "LOTSER" USING WS-LS
           IF CA-RETURN-CODE OF WS-LS > 8
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1001 " CA-MESSAGE OF WS-LS
               GO TO 1000-EXIT
           END-IF
           IF LS-TRACE-CNT = 0
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-1002 no movements for that identity"
           END-IF
           MOVE SPACES TO B-IDENTITY
           IF WS-SERIAL NOT = SPACES
               STRING "Serial " FUNCTION TRIM(WS-SERIAL)
                   DELIMITED BY SIZE INTO B-IDENTITY
           ELSE
               STRING "Lot " FUNCTION TRIM(WS-LOT)
                   DELIMITED BY SIZE INTO B-IDENTITY
           END-IF
           MOVE WS-ITEM TO B-ITEM
           PERFORM 1100-ITEM-DESC.
       1000-EXIT.
           EXIT.

       1050-RESET-LS SECTION.
       1050-START.
           INITIALIZE WS-LS
           MOVE WS-CO TO CA-COMPANY OF WS-LS
           MOVE WS-SITE TO CA-SITE OF WS-LS
           MOVE WS-USER TO CA-USER OF WS-LS
           MOVE WS-ASAT TO CA-BUSINESS-DATE OF WS-LS
           MOVE WS-ITEM TO LS-ITEM-CODE
           MOVE WS-LOT TO LS-LOT-CODE
           MOVE WS-SERIAL TO LS-SERIAL-NO
           MOVE WS-SITE TO LS-SITE
           MOVE WS-ASAT TO LS-AS-AT.
       1050-EXIT.
           EXIT.

       1100-ITEM-DESC SECTION.
       1100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Description FROM inv.Item WHERE "
                  "CompanyCode = ? AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-ITEM TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-OK
               MOVE ITM-COL(1) TO B-DESC
           END-IF.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "LOTTRACE" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0301 cannot build the spool path"
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           PERFORM 0350-FILL-HEADINGS
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0302 cannot open " WS-SPOOL-NAME
               GO TO 0300-EXIT
           END-IF
           MOVE "Y" TO WS-SPOOL-OPEN
           INITIATE R-LOTTRCE.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "LOTTRACE" TO RPT-REPORT-ID
           MOVE "Lot and serial traceability report" TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE WS-ASAT TO WS-DATE-N
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "As at " WS-DATE-DD "/" WS-DATE-MM "/"
                  WS-DATE-YYYY
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           MOVE SPACES TO RPT-SUBTITLE
           STRING "Trace direction " FUNCTION TRIM(WS-MODE)
               DELIMITED BY SIZE INTO RPT-SUBTITLE
           PERFORM 0360-READ-COMPANY.
       0350-EXIT.
           EXIT.

       0360-READ-COMPANY SECTION.
       0360-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Name, ISNULL(RegisteredNumber,'') FROM "
                  "core.Company WHERE CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-OK
               MOVE ITM-COL(1) TO RPT-COMPANY-NAME
               MOVE ITM-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       0360-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1200  ONE DETAIL LINE PER TRACED MOVEMENT.  A MOVEMENT THAT
      *>       INCREASED THE IDENTITY IS BACKWARD TRACE (WHERE IT CAME
      *>       FROM); A MOVEMENT THAT DECREASED IT IS FORWARD TRACE
      *>       (WHERE IT WENT).  HOLD AND RELEASE ARE NEITHER, SO THEY
      *>       ARE PRINTED AS QUALITY EVIDENCE WHATEVER THE MODE.
      *>----------------------------------------------------------------
       1200-PRINT-TRACE SECTION.
       1200-START.
           PERFORM VARYING WS-TX FROM 1 BY 1
                   UNTIL WS-TX > LS-TRACE-CNT
               MOVE WS-TX TO L-SEQ
               MOVE LS-T-TYPE(WS-TX) TO L-TYPE
               MOVE LS-T-QTY(WS-TX) TO L-QTY
               MOVE LS-T-SITE(WS-TX) TO L-SITE
               MOVE LS-T-LOCATION(WS-TX) TO L-LOCATION
               MOVE LS-T-DOC-TYPE(WS-TX) TO L-DOC-TYPE
               MOVE LS-T-DOC-REF(WS-TX) TO L-DOC-REF
               MOVE LS-T-USER(WS-TX) TO L-WHO
               MOVE LS-T-DATE(WS-TX) TO WS-DATE-N
               MOVE SPACES TO L-DATE
               STRING WS-DATE-DD "/" WS-DATE-MM "/" WS-DATE-YYYY
                   DELIMITED BY SIZE INTO L-DATE
               EVALUATE TRUE
                   WHEN LS-T-TYPE(WS-TX) = "HOLD"
                        OR LS-T-TYPE(WS-TX) = "RELEASE"
                       MOVE "Quality " TO L-DIRECTION
                       ADD 1 TO T-HOLDS
                   WHEN LS-T-QTY(WS-TX) < 0
                       MOVE "Forward " TO L-DIRECTION
                       COMPUTE T-OUT = T-OUT - LS-T-QTY(WS-TX)
                   WHEN OTHER
                       MOVE "Backward" TO L-DIRECTION
                       ADD LS-T-QTY(WS-TX) TO T-IN
               END-EVALUATE
               ADD 1 TO T-MOVES
               IF WS-MODE = "FORWARD" AND L-DIRECTION = "Backward"
                   CONTINUE
               ELSE
                   IF WS-MODE = "BACKWARD" AND L-DIRECTION = "Forward "
                       CONTINUE
                   ELSE
                       ADD 1 TO WS-ROWS-OUT
                       GENERATE DE-LINE
                   END-IF
               END-IF
           END-PERFORM.
       1200-EXIT.
           EXIT.

       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-LOTTRCE
           CLOSE RPT-FILE
           DISPLAY "STERP-I-0401 " WS-ROWS-OUT " traced movement(s)"
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0400-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "LOTTRACE" TO JL-JOB-NAME
           EVALUATE TRUE
               WHEN WS-RC = 0
                   MOVE "COMPLETE" TO JL-STATUS
               WHEN WS-RC = 4
                   MOVE "WARNING" TO JL-STATUS
               WHEN OTHER
                   MOVE "FAILED" TO JL-STATUS
           END-EVALUATE
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE WS-ROWS-OUT TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       0500-EXIT.
           EXIT.
