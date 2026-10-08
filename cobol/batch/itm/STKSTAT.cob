      *>================================================================
      *> PROGRAM-ID : STKSTAT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE (STOCK CONTROL)
      *> PURPOSE    : STOCK STATUS REPORT.  132 COLUMN REPORT WRITER
      *>              LISTING OF EVERY PHYSICAL STOCK BUCKET IN SCOPE
      *>              WITH ITS OWNER, CONDITION, LOT OR SERIAL AND
      *>              WHETHER IT IS ELIGIBLE TO BE USED.  THIRD PARTY
      *>              OWNED, IN TRANSIT, QUARANTINED AND EXPIRED
      *>              QUANTITIES ARE PRINTED SEPARATELY SO THAT THEY
      *>              CANNOT BE READ AS ORDINARY AVAILABLE STOCK.
      *> RUN AS     : STKSTAT CO=STC SITE=RDG ITEM=xxx USER=WHSE
      *>              [ASAT=yyyymmdd] [MODE=ALL|ELIGIBLE]
      *> OUTPUT     : run\spool\STKSTAT-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-05-14 RJH  CR0425 ORIGINAL - STOCK STATUS LISTING
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS IN THE HEADINGS
      *> 2001-04-02 KMP  CR1028 OWNERSHIP COLUMN AFTER THE CONSIGNMENT
      *>                 STOCK AUDIT FINDING
      *> 2026-10-05 STERP ITM-012, ITM-013 ELIGIBILITY COLUMN
      *>================================================================
      *> REQ: ITM-012, ITM-013, ITM-023
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STKSTAT.
       AUTHOR. R J HOLLIS.
       DATE-WRITTEN. 14/05/1996.
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
       FD  RPT-FILE REPORT IS R-STKSTAT.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ITMWORK.
       COPY STRPTWS.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-SITE                 PIC X(10) VALUE SPACES.
           05  WS-ITEM                 PIC X(40) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-ASAT                 PIC 9(8) VALUE 0.
           05  WS-MODE                 PIC X(10) VALUE SPACES.
       01  WS-RUN.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-MSG                  PIC X(120) VALUE SPACES.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
       01  WS-DET.
           05  D-ITEM                  PIC X(40) VALUE SPACES.
           05  D-DESC                  PIC X(34) VALUE SPACES.
           05  D-SITE                  PIC X(5) VALUE SPACES.
           05  D-WHS                   PIC X(6) VALUE SPACES.
           05  D-LOC                   PIC X(10) VALUE SPACES.
           05  D-LOT                   PIC X(16) VALUE SPACES.
           05  D-OWNER                 PIC X(10) VALUE SPACES.
           05  D-COND                  PIC X(11) VALUE SPACES.
           05  D-QTY                   PIC S9(11)V9(3) VALUE 0.
           05  D-RESV                  PIC S9(11)V9(3) VALUE 0.
           05  D-FREE                  PIC S9(11)V9(3) VALUE 0.
           05  D-ELIG                  PIC X(3) VALUE SPACES.
           05  D-WHY                   PIC X(24) VALUE SPACES.
           05  D-UOM                   PIC X(5) VALUE SPACES.
       01  WS-TOT.
           05  T-PHYSICAL              PIC S9(13)V9(3) VALUE 0.
           05  T-ELIGIBLE              PIC S9(13)V9(3) VALUE 0.
           05  T-RESERVED              PIC S9(13)V9(3) VALUE 0.
           05  T-THIRDPARTY            PIC S9(13)V9(3) VALUE 0.
           05  T-BUCKETS               PIC S9(7) VALUE 0.
       REPORT SECTION.
       RD  R-STKSTAT
           CONTROLS ARE FINAL D-ITEM
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 56
           FOOTING 58.
       COPY STRPTHDR.
       01  CH-ITEM TYPE IS CONTROL HEADING D-ITEM.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(5) VALUE "Item ".
               10  COLUMN 6    PIC X(40) SOURCE D-ITEM.
               10  COLUMN 48   PIC X(34) SOURCE D-DESC.
               10  COLUMN 84   PIC X(5) VALUE "Unit ".
               10  COLUMN 89   PIC X(5) SOURCE D-UOM.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(5) VALUE "Site ".
               10  COLUMN 9    PIC X(5) VALUE "Whse ".
               10  COLUMN 16   PIC X(8) VALUE "Location".
               10  COLUMN 27   PIC X(13) VALUE "Lot / serial ".
               10  COLUMN 44   PIC X(5) VALUE "Owner".
               10  COLUMN 55   PIC X(9) VALUE "Condition".
               10  COLUMN 70   PIC X(8) VALUE "Quantity".
               10  COLUMN 87   PIC X(8) VALUE "Reserved".
               10  COLUMN 104  PIC X(4) VALUE "Free".
               10  COLUMN 110  PIC X(3) VALUE "Elg".
               10  COLUMN 114  PIC X(19) VALUE "Not eligible because".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(66) VALUE ALL "-".
               10  COLUMN 69   PIC X(64) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(5) SOURCE D-SITE.
               10  COLUMN 9    PIC X(6) SOURCE D-WHS.
               10  COLUMN 16   PIC X(10) SOURCE D-LOC.
               10  COLUMN 27   PIC X(16) SOURCE D-LOT.
               10  COLUMN 44   PIC X(10) SOURCE D-OWNER.
               10  COLUMN 55   PIC X(11) SOURCE D-COND.
               10  COLUMN 67   PIC Z(10)9.999 SOURCE D-QTY.
               10  COLUMN 84   PIC Z(10)9.999 SOURCE D-RESV.
               10  COLUMN 98   PIC Z(7)9.999 SOURCE D-FREE.
               10  COLUMN 111  PIC X(3) SOURCE D-ELIG.
               10  COLUMN 115  PIC X(18) SOURCE D-WHY.
       01  CF-ITEM TYPE IS CONTROL FOOTING D-ITEM.
           05  LINE PLUS 1.
               10  COLUMN 27   PIC X(14) VALUE "Item totals   ".
               10  COLUMN 55   PIC X(11) VALUE "physical   ".
               10  COLUMN 67   PIC Z(10)9.999 SUM D-QTY.
               10  COLUMN 84   PIC Z(10)9.999 SUM D-RESV.
               10  COLUMN 98   PIC Z(7)9.999 SUM D-FREE.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(23)
                               VALUE "Report totals - owned:".
               10  COLUMN 26   PIC X(10) VALUE "physical".
               10  COLUMN 36   PIC Z(11)9.999 SOURCE T-PHYSICAL.
               10  COLUMN 53   PIC X(10) VALUE "eligible".
               10  COLUMN 63   PIC Z(11)9.999 SOURCE T-ELIGIBLE.
               10  COLUMN 80   PIC X(10) VALUE "reserved".
               10  COLUMN 90   PIC Z(11)9.999 SOURCE T-RESERVED.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(32)
                               VALUE "Third party owned in custody:".
               10  COLUMN 36   PIC Z(11)9.999 SOURCE T-THIRDPARTY.
               10  COLUMN 55   PIC X(9) VALUE "Buckets:".
               10  COLUMN 64   PIC Z(6)9 SOURCE T-BUCKETS.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(44) VALUE
                   "Eligible stock is owned, accepted, unexpired".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(34) VALUE
                   "and in an available location.".
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
           PERFORM 1000-PRODUCE-REPORT
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
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-ITEM
           END-IF
           IF CM-ASAT NOT = SPACES
               COMPUTE WS-ASAT = FUNCTION NUMVAL(CM-ASAT)
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "WHSE" TO WS-USER
           END-IF
           IF WS-ASAT = 0
               MOVE FUNCTION CURRENT-DATE TO WS-NOW
               COMPUTE WS-ASAT = FUNCTION NUMVAL(WS-NOW(1:8))
           END-IF
           IF WS-MODE = SPACES
               MOVE "ALL" TO WS-MODE
           END-IF
           DISPLAY "STERP-I-0100 STKSTAT stock status for "
                   FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "STKSTAT" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG.
       0200-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "STKSTAT" TO RP-REPORT-ID
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
           INITIATE R-STKSTAT.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "STKSTAT" TO RPT-REPORT-ID
           MOVE "Stock status report" TO RPT-TITLE
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
           IF WS-SITE = SPACES
               STRING "All sites" DELIMITED BY SIZE
                   INTO RPT-SUBTITLE
           ELSE
               STRING "Site " FUNCTION TRIM(WS-SITE)
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           END-IF
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
       1000-PRODUCE-REPORT SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC inv.usp_StockEnquiry ?, ?, ?, NULL, NULL, "
                  "NULL, NULL, NULL, NULL, NULL, ?, 0"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           IF WS-ITEM = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(2)
               MOVE SPACES TO SQLIO-PARM-VAL(2)
           ELSE
               MOVE WS-ITEM TO SQLIO-PARM-VAL(2)
               MOVE "N" TO SQLIO-PARM-NULL(2)
           END-IF
           IF WS-SITE = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(3)
               MOVE SPACES TO SQLIO-PARM-VAL(3)
           ELSE
               MOVE WS-SITE TO SQLIO-PARM-VAL(3)
               MOVE "N" TO SQLIO-PARM-NULL(3)
           END-IF
           MOVE WS-ASAT TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           MOVE 21 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1001 " SQLIO-ERRMSG(1:100)
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA ITM-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   PERFORM 1100-EMIT-LINE
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW.
       1000-EXIT.
           EXIT.

       1100-EMIT-LINE SECTION.
       1100-START.
           MOVE ITM-COL(6) TO D-ITEM
           MOVE ITM-COL(2) TO D-SITE
           MOVE ITM-COL(3) TO D-WHS
           MOVE ITM-COL(4) TO D-LOC
           MOVE SPACES TO D-LOT
           IF ITM-COL(9) NOT = SPACES
               MOVE ITM-COL(9) TO D-LOT
           ELSE
               MOVE ITM-COL(8) TO D-LOT
           END-IF
           MOVE SPACES TO D-OWNER
           IF ITM-COL(10) = "OWNED"
               MOVE "Owned" TO D-OWNER
           ELSE
               STRING FUNCTION TRIM(ITM-COL(10)) " "
                      FUNCTION TRIM(ITM-COL(11))
                   DELIMITED BY SIZE INTO D-OWNER
           END-IF
           MOVE ITM-COL(12) TO D-COND
           COMPUTE D-QTY = FUNCTION NUMVAL(ITM-COL(13))
           COMPUTE D-RESV = FUNCTION NUMVAL(ITM-COL(19))
           MOVE ITM-COL(15) TO D-UOM
           IF ITM-COL(20)(1:1) = "1"
               MOVE "Yes" TO D-ELIG
               COMPUTE D-FREE = D-QTY - D-RESV
               IF D-FREE < 0
                   MOVE 0 TO D-FREE
               END-IF
               ADD D-QTY TO T-ELIGIBLE
           ELSE
               MOVE "No " TO D-ELIG
               MOVE 0 TO D-FREE
           END-IF
           MOVE ITM-COL(21) TO D-WHY
           MOVE SPACES TO D-DESC
           IF ITM-COL(10) = "OWNED"
               ADD D-QTY TO T-PHYSICAL
           ELSE
               ADD D-QTY TO T-THIRDPARTY
           END-IF
           ADD D-RESV TO T-RESERVED
           ADD 1 TO T-BUCKETS WS-ROWS-OUT
           IF WS-MODE = "ELIGIBLE" AND D-ELIG = "No "
               GO TO 1100-EXIT
           END-IF
           PERFORM 1200-ITEM-DESC
           GENERATE DE-LINE.
       1100-EXIT.
           EXIT.

       1200-ITEM-DESC SECTION.
       1200-START.
      *>   the enquiry does not return the description, so it is read
      *>   once per item (cursor 2 is reserved for this lookup)
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Description FROM inv.Item WHERE "
                  "CompanyCode = ? AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE D-ITEM TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-OK
               MOVE ITM-COL(1) TO D-DESC
           END-IF
           MOVE 21 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR.
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-STKSTAT
           CLOSE RPT-FILE
           DISPLAY "STERP-I-0401 " WS-ROWS-OUT " stock bucket(s)"
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0400-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "STKSTAT" TO JL-JOB-NAME
           IF WS-RC = 0
               MOVE "COMPLETE" TO JL-STATUS
           ELSE
               MOVE "FAILED" TO JL-STATUS
           END-IF
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE WS-ROWS-OUT TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       0500-EXIT.
           EXIT.
