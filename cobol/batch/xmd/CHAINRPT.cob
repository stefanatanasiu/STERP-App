      *>================================================================
      *> PROGRAM-ID : CHAINRPT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : CROSS MODULE CONTROLS AND REPORTING (XMD)
      *> PURPOSE    : PRINTED BUSINESS CHAIN TRACE.  CALLS CHAINTRC
      *>              FOR ONE DOCUMENT AND LISTS THE ORDERED STEPS OF
      *>              ITS ORDER TO CASH, PROCURE TO PAY OR ASSEMBLY
      *>              CHAIN, WITH THE OPERATIONAL STEPS, THE
      *>              FINANCIAL EFFECTS AND THE SEPARATE RETURN,
      *>              CREDIT AND REVERSAL CONSEQUENCES DISTINGUISHED.
      *> RUN AS     : CHAINRPT CO=STC TYPE=ORDER REF=SO-00012
      *>              USER=SALESMGR
      *> OUTPUT     : run\spool\CHAINRPT-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, CHAINTRC, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-07-14 RJH  CR0655 PRINTED TRAIL FOR THE AUDITORS
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS THROUGHOUT
      *> 2026-10-06 STERP XMD-001..004 CHAIN TRACE LISTING
      *>================================================================
      *> REQ: XMD-001, XMD-002, XMD-003, XMD-004
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHAINRPT.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 14/07/1997.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO DYNAMIC XB-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS XB-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-CHAINRPT.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY XMDBATWK.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARMGET==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXCPRAIS==.
       COPY CHAINTRCCA REPLACING ==DFHCOMMAREA== BY ==WS-CT-CA==.
       01  WS-DET.
           05  D-SEQ                   PIC 9(2) VALUE 0.
           05  D-STAGE                 PIC X(24) VALUE SPACES.
           05  D-DOCT                  PIC X(12) VALUE SPACES.
           05  D-REF                   PIC X(30) VALUE SPACES.
           05  D-DATE                  PIC X(10) VALUE SPACES.
           05  D-STATUS                PIC X(12) VALUE SPACES.
           05  D-QTY                   PIC S9(11)V9(3) VALUE 0.
           05  D-VALUE                 PIC S9(13)V99 VALUE 0.
           05  D-KIND                  PIC X(10) VALUE SPACES.
           05  D-NOTE                  PIC X(60) VALUE SPACES.
       01  WS-SCOPE.
           05  S-TYPE                  PIC X(12) VALUE SPACES.
           05  S-REF                   PIC X(40) VALUE SPACES.
           05  S-CHAIN                 PIC X(40) VALUE SPACES.
           05  S-PARTY                 PIC X(20) VALUE SPACES.
           05  S-STATE                 PIC X(28) VALUE SPACES.
       REPORT SECTION.
       RD  R-CHAINRPT
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 8 LAST DETAIL 52
           FOOTING 58.
       COPY STRPTHDR.
       01  DE-SCOPE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(14) VALUE "Document type ".
               10  COLUMN 15   PIC X(12) SOURCE S-TYPE.
               10  COLUMN 29   PIC X(10) VALUE "Reference ".
               10  COLUMN 39   PIC X(40) SOURCE S-REF.
               10  COLUMN 82   PIC X(6) VALUE "Chain ".
               10  COLUMN 88   PIC X(40) SOURCE S-CHAIN.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(14) VALUE "Counterparty  ".
               10  COLUMN 15   PIC X(20) SOURCE S-PARTY.
               10  COLUMN 39   PIC X(28) SOURCE S-STATE.
               10  COLUMN 70   PIC X(60) VALUE
                   "One step complete does not imply the next.".
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(3) VALUE "Seq".
               10  COLUMN 6    PIC X(5) VALUE "Stage".
               10  COLUMN 32   PIC X(4) VALUE "Type".
               10  COLUMN 46   PIC X(9) VALUE "Reference".
               10  COLUMN 78   PIC X(4) VALUE "Date".
               10  COLUMN 90   PIC X(6) VALUE "Status".
               10  COLUMN 104  PIC X(8) VALUE "Quantity".
               10  COLUMN 118  PIC X(9) VALUE "Value GBP".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-STEP TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC Z9 SOURCE D-SEQ.
               10  COLUMN 6    PIC X(24) SOURCE D-STAGE.
               10  COLUMN 32   PIC X(12) SOURCE D-DOCT.
               10  COLUMN 46   PIC X(30) SOURCE D-REF.
               10  COLUMN 78   PIC X(10) SOURCE D-DATE.
               10  COLUMN 90   PIC X(12) SOURCE D-STATUS.
               10  COLUMN 101  PIC ZZZ,ZZ9.999 SOURCE D-QTY.
               10  COLUMN 114  PIC Z,ZZZ,ZZ9.99- SOURCE D-VALUE.
           05  LINE PLUS 1.
               10  COLUMN 8    PIC X(10) SOURCE D-KIND.
               10  COLUMN 19   PIC X(60) SOURCE D-NOTE.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(14) VALUE "Steps traced  ".
               10  COLUMN 16   PIC ZZ9 SOURCE XB-ROWS-OUT.
               10  COLUMN 24   PIC X(34) VALUE
                   "Separate reversal or credit steps ".
               10  COLUMN 58   PIC ZZ9 SOURCE XB-DIFFS.
               10  COLUMN 66   PIC X(20) SOURCE RPT-STATUS-TEXT.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(62) VALUE
                   "A physical return is not a financial credit and".
               10  COLUMN 50   PIC X(60) VALUE
                   "neither is a replacement order (XMD-004)".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           MOVE "CHAINRPT" TO RPT-REPORT-ID
           MOVE "Business chain trace" TO RPT-TITLE
           MOVE "Order to cash, procure to pay and assembly chains"
               TO RPT-SUBTITLE
           PERFORM 9100-JOB-START THRU 9100-EXIT
           PERFORM 9200-OPEN-SPOOL THRU 9200-EXIT
           IF XB-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 1000-TRACE THRU 1000-EXIT
           INITIATE R-CHAINRPT
           GENERATE DE-SCOPE
           PERFORM 2000-LIST THRU 2000-EXIT
           TERMINATE R-CHAINRPT
           PERFORM 9350-CLOSE-SPOOL THRU 9350-EXIT.
       0000-FINISH.
           PERFORM 9150-JOB-END THRU 9150-EXIT
           MOVE XB-RC TO RETURN-CODE
           STOP RUN.

       0100-READ-ARGS.
           PERFORM 9500-STD-ARGS THRU 9500-EXIT
           MOVE "TYPE" TO XB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           IF XB-TEXT = SPACES
               MOVE "ORDER" TO XB-TYPE
           ELSE
               MOVE FUNCTION UPPER-CASE(XB-TEXT(1:12)) TO XB-TYPE
           END-IF
           MOVE "REF" TO XB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           MOVE XB-TEXT(1:40) TO XB-CODE
           MOVE XB-TYPE TO S-TYPE
           MOVE XB-CODE TO S-REF
           DISPLAY "STERP-I-0100 CHAINRPT " FUNCTION TRIM(XB-TYPE)
                   " " FUNCTION TRIM(XB-CODE).
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: XMD-001, XMD-002, XMD-003  the trace itself belongs to
      *> the LINKable enquiry, so the client and the printed listing
      *> can never disagree about the same chain
      *>----------------------------------------------------------------
       1000-TRACE.
           INITIALIZE WS-CT-CA
           MOVE "TRACE" TO CA-FUNCTION OF WS-CT-CA
           MOVE XB-CO TO CA-COMPANY OF WS-CT-CA
           MOVE XB-SITE TO CA-SITE OF WS-CT-CA
           MOVE XB-USER TO CA-USER OF WS-CT-CA
           MOVE XB-ASAT TO CA-BUSINESS-DATE OF WS-CT-CA
           MOVE XB-TYPE TO CT-DOC-TYPE
           MOVE XB-CODE TO CT-DOC-REF
           CALL "CHAINTRC" USING WS-CT-CA
           MOVE CT-CHAIN-NAME TO S-CHAIN
           MOVE CT-PARTY TO S-PARTY
           MOVE CT-REVERSALS TO XB-DIFFS
           EVALUATE TRUE
               WHEN CT-BROKEN = "Y"
                   MOVE "INCOMPLETE CHAIN" TO S-STATE
                   MOVE "INCOMPLETE" TO RPT-STATUS-TEXT
               WHEN CT-COMPLETE = "Y"
                   MOVE "CHAIN COMPLETE" TO S-STATE
                   MOVE "COMPLETE" TO RPT-STATUS-TEXT
               WHEN OTHER
                   MOVE "PARTIALLY COMPLETE" TO S-STATE
                   MOVE "PARTIAL" TO RPT-STATUS-TEXT
           END-EVALUATE
           IF CA-RETURN-CODE OF WS-CT-CA > 4
               MOVE CA-RETURN-CODE OF WS-CT-CA TO XB-RC
               DISPLAY "STERP-W-1001 "
                       CA-REASON-CODE OF WS-CT-CA " "
                       FUNCTION TRIM(CA-MESSAGE OF WS-CT-CA)
           END-IF.
       1000-EXIT.
           EXIT.

       2000-LIST.
           PERFORM VARYING XB-I FROM 1 BY 1
                   UNTIL XB-I > CT-STEP-CNT
               MOVE CT-SEQ(XB-I) TO D-SEQ
               MOVE CT-STAGE(XB-I) TO D-STAGE
               MOVE CT-DOC-T(XB-I) TO D-DOCT
               MOVE CT-REF(XB-I)(1:30) TO D-REF
               MOVE CT-STATUS(XB-I) TO D-STATUS
               MOVE CT-QTY(XB-I) TO D-QTY
               MOVE CT-VALUE(XB-I) TO D-VALUE
               MOVE CT-KIND(XB-I) TO D-KIND
               MOVE CT-NOTE(XB-I) TO D-NOTE
               MOVE CT-DATE(XB-I) TO XB-NUM-DATE
               PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
               MOVE XB-UK TO D-DATE
               ADD 1 TO XB-ROWS-OUT
               GENERATE DE-STEP
           END-PERFORM
           DISPLAY "STERP-I-2001 " XB-ROWS-OUT " chain step(s), "
                   XB-DIFFS " reversal or credit step(s)".
       2000-EXIT.
           EXIT.

       COPY XMDBATPR.
