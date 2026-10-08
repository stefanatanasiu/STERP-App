      *>================================================================
      *> PROGRAM-ID : OPENSTK
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE (STOCK CONTROL)
      *> PURPOSE    : OPENING STOCK LOAD.  READS THE OPENING STOCK
      *>              FILE PREPARED BY THE IMPLEMENTATION TEAM, PROVES
      *>              EVERY ROW (ITEM, LOCATION, QUANTITY, OWNERSHIP,
      *>              CONDITION, TRACEABILITY AND VALUATION EVIDENCE)
      *>              AND POSTS THE ACCEPTED ROWS AS OPENING STOCK
      *>              MOVEMENTS THROUGH STKMOVE, WHICH OWNS THE
      *>              CONTRACT inv.usp_PostStockMovement.  A BALANCE IS
      *>              NEVER WRITTEN DIRECTLY.  REJECTED ROWS GO TO AN
      *>              EXCEPTIONS REPORT WITH THE REASON, SO THE LOAD
      *>              CAN BE CORRECTED AND RE-RUN: THE CORRELATION KEY
      *>              STOPS A ROW BEING LOADED TWICE.
      *> RUN AS     : OPENSTK CO=STC SITE=RDG USER=STKACCT
      *>              IN=<path> [ASAT=yyyymmdd] [MODE=PREVIEW|COMMIT]
      *> INPUT      : CSV - ITEM,SITE,WHSE,LOC,LOT,SERIAL,CONDITION,
      *>                    QTY,UOM,UNITCOST,EXPIRY,OWNERTYPE,
      *>                    OWNERCODE,REFERENCE
      *> OUTPUT     : run\spool\OPENSTK-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, STKMOVE, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-04-02 RJH  CR0418 ORIGINAL - OPENING BALANCE CONVERSION
      *> 1997-02-26 DJM  CR0688 REJECT FILE BECAME A PRINTED REPORT
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS ON EXPIRY DATES
      *> 2014-10-09 ANW  CR3012 CORRELATION KEY STOPS A DOUBLE LOAD
      *> 2026-10-05 STERP ITM-015, ITM-024 THROUGH STKMOVE
      *>================================================================
      *> REQ: ITM-015, ITM-024
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OPENSTK.
       AUTHOR. R J HOLLIS.
       DATE-WRITTEN. 02/04/1996.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO WS-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
           SELECT LOAD-FILE ASSIGN TO WS-INPUT-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-IN-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-OPENSTK.
       FD  LOAD-FILE.
       01  LOAD-REC                    PIC X(300).
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ITMWORK.
       COPY STRPTWS.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY STKMOVECA REPLACING ==DFHCOMMAREA== BY ==WS-SM==.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-INPUT-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-IN-STATUS                PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-SITE                 PIC X(10) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-ASAT                 PIC 9(8) VALUE 0.
           05  WS-MODE                 PIC X(10) VALUE SPACES.
       01  WS-RUN.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-LINE-NO              PIC S9(7) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-EOF                  PIC X VALUE "N".
               88  WS-IN-EOF                     VALUE "Y".
           05  WS-BAD-ROW              PIC X VALUE "N".
           05  WS-REASON               PIC X(8) VALUE SPACES.
           05  WS-WHY                  PIC X(70) VALUE SPACES.
      *>   COM-015.  Every opening balance line is its own idempotent
      *>   instruction, so its correlation id has to stay unique.  The
      *>   natural key alone did not: it was simply cut to the 36
      *>   characters the commarea holds, and two long item codes that
      *>   agreed for the first 36 characters shared one key, so the
      *>   second line was treated as a replay and no stock was loaded
      *>   for it (CR2741).  The file line number is appended to a
      *>   shortened stem so it can never be truncated away.
           05  WS-CORR-STEM            PIC X(27) VALUE SPACES.
       01  WS-FLD.
           05  F-ITEM                  PIC X(40) VALUE SPACES.
           05  F-SITE                  PIC X(10) VALUE SPACES.
           05  F-WHSE                  PIC X(20) VALUE SPACES.
           05  F-LOC                   PIC X(20) VALUE SPACES.
           05  F-LOT                   PIC X(30) VALUE SPACES.
           05  F-SERIAL                PIC X(40) VALUE SPACES.
           05  F-COND                  PIC X(12) VALUE SPACES.
           05  F-QTY-X                 PIC X(30) VALUE SPACES.
           05  F-UOM                   PIC X(10) VALUE SPACES.
           05  F-COST-X                PIC X(30) VALUE SPACES.
           05  F-EXPIRY-X              PIC X(10) VALUE SPACES.
           05  F-OWNTYPE               PIC X(10) VALUE SPACES.
           05  F-OWNCODE               PIC X(20) VALUE SPACES.
           05  F-REF                   PIC X(40) VALUE SPACES.
       01  WS-NUMS.
           05  N-QTY                   PIC S9(13)V9(6) VALUE 0.
           05  N-COST                  PIC S9(11)V9(6) VALUE 0.
           05  N-EXPIRY                PIC 9(8) VALUE 0.
           05  N-VALUE                 PIC S9(13)V99 VALUE 0.
       01  WS-DET.
           05  X-LINE                  PIC 9(7) VALUE 0.
           05  X-ITEM                  PIC X(40) VALUE SPACES.
           05  X-LOC                   PIC X(24) VALUE SPACES.
           05  X-LOT                   PIC X(20) VALUE SPACES.
           05  X-QTY                   PIC S9(11)V9(3) VALUE 0.
           05  X-REASON                PIC X(8) VALUE SPACES.
           05  X-WHY                   PIC X(70) VALUE SPACES.
       01  WS-TOT.
           05  T-READ                  PIC S9(7) VALUE 0.
           05  T-POSTED                PIC S9(7) VALUE 0.
           05  T-REJECTED              PIC S9(7) VALUE 0.
           05  T-DUPLICATE             PIC S9(7) VALUE 0.
           05  T-QTY                   PIC S9(13)V9(3) VALUE 0.
           05  T-VALUE                 PIC S9(13)V99 VALUE 0.
       REPORT SECTION.
       RD  R-OPENSTK
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 11 LAST DETAIL 54
           FOOTING 56.
       COPY STRPTHDR.
       01  RH-COLHEAD TYPE IS REPORT HEADING.
           05  LINE 8.
               10  COLUMN 1    PIC X(56) VALUE
                   "Rows the load could not post - correct and re-run.".
           05  LINE 9.
               10  COLUMN 1    PIC X(4) VALUE "Line".
               10  COLUMN 10   PIC X(4) VALUE "Item".
               10  COLUMN 52   PIC X(8) VALUE "Location".
               10  COLUMN 78   PIC X(11) VALUE "Lot/serial ".
               10  COLUMN 100  PIC X(8) VALUE "Quantity".
               10  COLUMN 115  PIC X(6) VALUE "Reason".
           05  LINE 10.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC Z(6)9 SOURCE X-LINE.
               10  COLUMN 10   PIC X(40) SOURCE X-ITEM.
               10  COLUMN 52   PIC X(24) SOURCE X-LOC.
               10  COLUMN 78   PIC X(20) SOURCE X-LOT.
               10  COLUMN 99   PIC Z(8)9.999 SOURCE X-QTY.
               10  COLUMN 115  PIC X(8) SOURCE X-REASON.
           05  LINE PLUS 1.
               10  COLUMN 10   PIC X(70) SOURCE X-WHY.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(12) VALUE "Rows read:".
               10  COLUMN 14   PIC Z(6)9 SOURCE T-READ.
               10  COLUMN 26   PIC X(8) VALUE "Posted:".
               10  COLUMN 34   PIC Z(6)9 SOURCE T-POSTED.
               10  COLUMN 46   PIC X(10) VALUE "Rejected:".
               10  COLUMN 56   PIC Z(6)9 SOURCE T-REJECTED.
               10  COLUMN 68   PIC X(11) VALUE "Duplicate:".
               10  COLUMN 79   PIC Z(6)9 SOURCE T-DUPLICATE.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(18) VALUE "Opening quantity:".
               10  COLUMN 20   PIC Z(12)9.999 SOURCE T-QTY.
               10  COLUMN 40   PIC X(15) VALUE "Opening value:".
               10  COLUMN 55   PIC Z(12)9.99 SOURCE T-VALUE.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(60) VALUE
                   "Opening stock is posted as a movement, never as an".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(60) VALUE
                   "edited balance, so layers tie to stock control.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-READ-ARGS
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0200-JOB-START
           PERFORM 0300-OPEN-SPOOL
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 1000-LOAD-FILE
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
           MOVE "IN" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-INPUT-NAME
           END-IF
           IF CM-ASAT NOT = SPACES
               COMPUTE WS-ASAT = FUNCTION NUMVAL(CM-ASAT)
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "STKACCT" TO WS-USER
           END-IF
           IF WS-MODE = SPACES
               MOVE "PREVIEW" TO WS-MODE
           END-IF
           IF WS-ASAT = 0
               MOVE FUNCTION CURRENT-DATE TO WS-NOW
               COMPUTE WS-ASAT = FUNCTION NUMVAL(WS-NOW(1:8))
           END-IF
           IF WS-INPUT-NAME = SPACES
               INITIALIZE WS-RUNPATH
               MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
               MOVE "EXTRACTS" TO RP-FOLDER
               MOVE "OPENSTK" TO RP-REPORT-ID
               MOVE WS-CO TO RP-COMPANY-REF
               MOVE "csv" TO RP-EXTENSION
               MOVE "N" TO RP-USE-TIMESTAMP
               CALL "RUNPATH" USING WS-RUNPATH
               MOVE RP-FULL-PATH TO WS-INPUT-NAME
           END-IF
           IF WS-MODE NOT = "PREVIEW" AND WS-MODE NOT = "COMMIT"
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0101 MODE must be PREVIEW or COMMIT"
               GO TO 0100-EXIT
           END-IF
           DISPLAY "STERP-I-0100 OPENSTK " FUNCTION TRIM(WS-MODE)
                   " load from " FUNCTION TRIM(WS-INPUT-NAME).
       0100-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "OPENSTK" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG.
       0200-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "OPENSTK" TO RP-REPORT-ID
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
           OPEN INPUT LOAD-FILE
           IF WS-IN-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0303 cannot read "
                       FUNCTION TRIM(WS-INPUT-NAME)
               GO TO 0300-EXIT
           END-IF
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0302 cannot open " WS-SPOOL-NAME
               GO TO 0300-EXIT
           END-IF
           INITIATE R-OPENSTK.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "OPENSTK" TO RPT-REPORT-ID
           MOVE "Opening stock load - exceptions" TO RPT-TITLE
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
           STRING "Opening at " WS-DATE-DD "/" WS-DATE-MM "/"
                  WS-DATE-YYYY
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           MOVE SPACES TO RPT-SUBTITLE
           STRING "Mode " FUNCTION TRIM(WS-MODE)
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
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
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
       1000-LOAD-FILE SECTION.
       1000-START.
           MOVE "N" TO WS-EOF
           PERFORM UNTIL WS-IN-EOF
               READ LOAD-FILE
                   AT END MOVE "Y" TO WS-EOF
                   NOT AT END PERFORM 1100-ONE-ROW
               END-READ
           END-PERFORM
           CLOSE LOAD-FILE.
       1000-EXIT.
           EXIT.

       1100-ONE-ROW SECTION.
       1100-START.
           ADD 1 TO WS-LINE-NO
           IF LOAD-REC = SPACES
               GO TO 1100-EXIT
           END-IF
           IF LOAD-REC(1:1) = "*" OR LOAD-REC(1:4) = "ITEM"
               GO TO 1100-EXIT
           END-IF
           ADD 1 TO T-READ
           PERFORM 1200-SPLIT-ROW
           PERFORM 1300-VALIDATE-ROW
           IF WS-BAD-ROW = "Y"
               PERFORM 1800-REJECT
               GO TO 1100-EXIT
           END-IF
           PERFORM 1400-POST-ROW.
       1100-EXIT.
           EXIT.

       1200-SPLIT-ROW SECTION.
       1200-START.
           MOVE SPACES TO F-ITEM F-SITE F-WHSE F-LOC F-LOT F-SERIAL
           MOVE SPACES TO F-COND F-QTY-X F-UOM F-COST-X F-EXPIRY-X
           MOVE SPACES TO F-OWNTYPE F-OWNCODE F-REF
           UNSTRING LOAD-REC DELIMITED BY ","
               INTO F-ITEM F-SITE F-WHSE F-LOC F-LOT F-SERIAL
                    F-COND F-QTY-X F-UOM F-COST-X F-EXPIRY-X
                    F-OWNTYPE F-OWNCODE F-REF
           END-UNSTRING
           IF F-SITE = SPACES
               MOVE WS-SITE TO F-SITE
           END-IF
           IF F-COND = SPACES
               MOVE "ACCEPTED" TO F-COND
           END-IF
           IF F-OWNTYPE = SPACES
               MOVE "OWNED" TO F-OWNTYPE
           END-IF.
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1300  FIELD LEVEL PROVING.  ANYTHING THAT NEEDS THE DATABASE
      *>       (ITEM STATE, CLASS, LOCATION, QUANTUM, LOT RULES) IS
      *>       LEFT TO STKMOVE SO THAT THE LOAD AND THE ONLINE SCREENS
      *>       CANNOT DRIFT APART.  CR0688.
      *>----------------------------------------------------------------
       1300-VALIDATE-ROW SECTION.
       1300-START.
           MOVE "N" TO WS-BAD-ROW
           MOVE SPACES TO WS-REASON WS-WHY
           MOVE 0 TO N-QTY N-COST N-EXPIRY N-VALUE
           IF F-ITEM = SPACES
               MOVE "OPN0001" TO WS-REASON
               MOVE "Item code is missing on the opening row"
                   TO WS-WHY
               MOVE "Y" TO WS-BAD-ROW
               GO TO 1300-EXIT
           END-IF
           IF F-LOC = SPACES
               MOVE "OPN0002" TO WS-REASON
               MOVE "Location is required for opening stock"
                   TO WS-WHY
               MOVE "Y" TO WS-BAD-ROW
               GO TO 1300-EXIT
           END-IF
           IF F-QTY-X = SPACES
               MOVE "OPN0003" TO WS-REASON
               MOVE "Opening quantity is missing" TO WS-WHY
               MOVE "Y" TO WS-BAD-ROW
               GO TO 1300-EXIT
           END-IF
           COMPUTE N-QTY = FUNCTION NUMVAL(F-QTY-X)
           IF N-QTY <= 0
               MOVE "OPN0003" TO WS-REASON
               MOVE "Opening quantity must be greater than zero"
                   TO WS-WHY
               MOVE "Y" TO WS-BAD-ROW
               GO TO 1300-EXIT
           END-IF
           IF F-COST-X NOT = SPACES
               COMPUTE N-COST = FUNCTION NUMVAL(F-COST-X)
           END-IF
           IF N-COST <= 0 AND F-OWNTYPE = "OWNED"
               MOVE "OPN0004" TO WS-REASON
               MOVE "Owned opening stock needs valuation evidence"
                   TO WS-WHY
               MOVE "Y" TO WS-BAD-ROW
               GO TO 1300-EXIT
           END-IF
           IF F-EXPIRY-X NOT = SPACES
               COMPUTE N-EXPIRY = FUNCTION NUMVAL(F-EXPIRY-X)
               IF N-EXPIRY < 19000101 OR N-EXPIRY > 29991231
                   MOVE "OPN0005" TO WS-REASON
                   MOVE "Expiry date is not a valid date"
                       TO WS-WHY
                   MOVE "Y" TO WS-BAD-ROW
                   GO TO 1300-EXIT
               END-IF
           END-IF
           IF F-OWNTYPE NOT = "OWNED" AND F-OWNCODE = SPACES
               MOVE "OPN0006" TO WS-REASON
               MOVE "Third party stock must name its owner"
                   TO WS-WHY
               MOVE "Y" TO WS-BAD-ROW
           END-IF.
       1300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1400  POST THROUGH STKMOVE.  THE CORRELATION KEY IS BUILT FROM
      *>       THE NATURAL KEY OF THE ROW, SO A SECOND RUN OF THE SAME
      *>       FILE IS RECOGNISED AS A DUPLICATE INSTEAD OF DOUBLING
      *>       THE OPENING BALANCE (ITM-024, CR3012).
      *>----------------------------------------------------------------
       1400-POST-ROW SECTION.
       1400-START.
           INITIALIZE WS-SM
           MOVE WS-CO TO CA-COMPANY OF WS-SM
           MOVE F-SITE TO CA-SITE OF WS-SM
           MOVE WS-USER TO CA-USER OF WS-SM
           MOVE WS-ASAT TO CA-BUSINESS-DATE OF WS-SM
           MOVE "OPENING" TO CA-FUNCTION OF WS-SM
           MOVE SPACES TO WS-TEXT
           STRING "OPENSTK|" FUNCTION TRIM(WS-CO) "|"
                  FUNCTION TRIM(F-ITEM) "|"
                  FUNCTION TRIM(F-SITE) "|"
                  FUNCTION TRIM(F-LOC) "|"
                  FUNCTION TRIM(F-LOT)
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT(1:27) TO WS-CORR-STEM
           MOVE WS-LINE-NO TO WS-INT-ED
           MOVE SPACES TO CA-CORRELATION-ID OF WS-SM
           STRING FUNCTION TRIM(WS-CORR-STEM) "#"
                  FUNCTION TRIM(WS-INT-ED)
               DELIMITED BY SIZE INTO CA-CORRELATION-ID OF WS-SM
           MOVE F-ITEM TO SM-ITEM-CODE
           MOVE F-SITE TO SM-SITE
           MOVE F-WHSE TO SM-WAREHOUSE
           MOVE F-LOC TO SM-LOCATION
           MOVE F-LOT TO SM-LOT-CODE
           MOVE F-SERIAL TO SM-SERIAL-NO
           MOVE F-COND TO SM-CONDITION
           MOVE F-OWNTYPE TO SM-OWNER-TYPE
           MOVE F-OWNCODE TO SM-OWNER-CODE
           MOVE N-QTY TO SM-QUANTITY
           MOVE F-UOM TO SM-UOM-CODE
           MOVE N-COST TO SM-UNIT-COST
           MOVE N-EXPIRY TO SM-EXPIRY-DATE
           MOVE WS-ASAT TO SM-MOVEMENT-DATE
           MOVE WS-ASAT TO SM-POSTING-DATE
           MOVE "OPENING" TO SM-REASON-CODE
           MOVE "ITM" TO SM-SOURCE-MODULE
           MOVE "OPENING STOCK" TO SM-SOURCE-DOC-TYPE
           MOVE F-REF TO SM-SOURCE-DOC-REF
           MOVE SPACES TO SM-REASON
           MOVE WS-LINE-NO TO WS-INT-ED
           STRING "Opening stock load line "
                  FUNCTION TRIM(WS-INT-ED)
               DELIMITED BY SIZE INTO SM-REASON
           IF WS-MODE = "PREVIEW"
               MOVE "Not posted - PREVIEW run" TO WS-WHY
               MOVE "OPN0007" TO WS-REASON
               ADD 1 TO T-REJECTED
               PERFORM 1800-REJECT
               GO TO 1400-EXIT
           END-IF
           CALL "STKMOVE" USING WS-SM
           IF CA-RETURN-CODE OF WS-SM < 8
               ADD 1 TO T-POSTED WS-ROWS-OUT
               ADD SM-STOCK-QTY TO T-QTY
               COMPUTE N-VALUE ROUNDED = SM-STOCK-QTY * N-COST
               ADD N-VALUE TO T-VALUE
               IF SM-DUPLICATE = "Y"
                   ADD 1 TO T-DUPLICATE
                   MOVE "OPN0008" TO WS-REASON
                   MOVE "Already loaded - duplicate opening receipt"
                       TO WS-WHY
                   PERFORM 1800-REJECT
               END-IF
           ELSE
               MOVE CA-REASON-CODE OF WS-SM TO WS-REASON
               MOVE CA-MESSAGE OF WS-SM TO WS-WHY
               ADD 1 TO T-REJECTED
               PERFORM 1800-REJECT
           END-IF.
       1400-EXIT.
           EXIT.

       1800-REJECT SECTION.
       1800-START.
           IF WS-BAD-ROW = "Y"
               ADD 1 TO T-REJECTED
           END-IF
           MOVE WS-LINE-NO TO X-LINE
           MOVE F-ITEM TO X-ITEM
           MOVE SPACES TO X-LOC
           STRING FUNCTION TRIM(F-SITE) "/" FUNCTION TRIM(F-LOC)
               DELIMITED BY SIZE INTO X-LOC
           MOVE SPACES TO X-LOT
           IF F-SERIAL NOT = SPACES
               MOVE F-SERIAL TO X-LOT
           ELSE
               MOVE F-LOT TO X-LOT
           END-IF
           MOVE N-QTY TO X-QTY
           MOVE WS-REASON TO X-REASON
           MOVE WS-WHY TO X-WHY
           GENERATE DE-LINE.
       1800-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-OPENSTK
           CLOSE RPT-FILE
           IF T-REJECTED > 0
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
               DISPLAY "STERP-W-0402 " T-REJECTED " row(s) rejected"
           END-IF
           DISPLAY "STERP-I-0403 " T-READ " row(s) read, "
                   T-POSTED " posted"
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0400-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "OPENSTK" TO JL-JOB-NAME
           EVALUATE TRUE
               WHEN WS-RC = 0
                   MOVE "COMPLETE" TO JL-STATUS
               WHEN WS-RC = 4
                   MOVE "WARNING" TO JL-STATUS
               WHEN OTHER
                   MOVE "FAILED" TO JL-STATUS
           END-EVALUATE
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE T-POSTED TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       0500-EXIT.
           EXIT.
