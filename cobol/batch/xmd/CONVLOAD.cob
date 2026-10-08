      *>================================================================
      *> PROGRAM-ID : CONVLOAD
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : CROSS MODULE CONTROLS AND REPORTING (XMD)
      *> PURPOSE    : LEGACY CONVERSION LOAD.  READS A FIXED WIDTH
      *>              EXTRACT FROM THE OLD SYSTEM (data\legacy\*.DAT)
      *>              AND LOADS IT AT A DATED CUT OVER.  FOUR RECORD
      *>              TYPES ARE UNDERSTOOD: AROP OPENING DEBTOR OPEN
      *>              ITEMS, APOP OPENING CREDITOR OPEN ITEMS, STKB
      *>              OPENING STOCK BALANCES AND GLTB THE OPENING
      *>              TRIAL BALANCE.  THE FILE IS SORTED ON TYPE AND
      *>              NATURAL KEY SO THAT DUPLICATES INSIDE THE FILE
      *>              ARE ADJACENT AND CANNOT BOTH BE ACCEPTED.
      *>              PREVIEW VALIDATES AND LISTS EVERY RECORD WITH
      *>              ITS OWN ACCEPT OR REJECT REASON AND CHANGES
      *>              NOTHING.  COMMIT WRITES THE BATCH AND ITS ROWS
      *>              TO THE IMPORT STAGING TABLES AND THE OPENING
      *>              TRIAL BALANCE TO fin.OpeningBalance.  THE BATCH
      *>              REFERENCE IS DERIVED FROM THE FILE NAME AND THE
      *>              CUT OVER DATE, SO A RESTARTED RUN RECOGNISES
      *>              THE ROWS IT ALREADY LOADED AND SKIPS THEM
      *>              INSTEAD OF LOADING THEM TWICE.
      *> RUN AS     : CONVLOAD CO=CAL FILE=data\legacy\CALAROPN.DAT
      *>              CUTOVER=20260331 MODE=PREVIEW USER=FINCTRL
      *> OUTPUT     : run\spool\CONVLOAD-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, PARMGET, EXCPRAIS,
      *>              SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1993-11-08 DJM  CR0222 ORIGINAL CONVERSION LOAD FOR THE 1994
      *>                 LEDGER CUT OVER
      *> 1995-02-20 RJH  CR0361 PREVIEW PASS ADDED - THE 1994 LOAD WAS
      *>                 RUN TWICE AND NOBODY NOTICED FOR A WEEK
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS THROUGHOUT
      *> 2026-10-06 STERP XMD-007 PER RECORD OUTCOME AND RESTART
      *>================================================================
      *> REQ: XMD-007, XMD-008, XMD-011, COM-018, COM-023, FIN-024,
      *> REQ: AR-021, AP-024, ITM-024
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONVLOAD.
       AUTHOR. D J MILLER.
       DATE-WRITTEN. 08/11/1993.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO DYNAMIC XB-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS XB-FILE-STATUS.
           SELECT LEG-FILE ASSIGN TO DYNAMIC WS-LEG-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LEG-STATUS.
           SELECT SORT-FILE ASSIGN TO "CONVSORT".
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-CONVLOAD.
       FD  LEG-FILE.
       01  LEG-REC                     PIC X(130).
       SD  SORT-FILE.
       01  SORT-REC.
           05  SR-TYPE                 PIC X(4).
           05  SR-KEY                  PIC X(60).
           05  SR-SEQ                  PIC 9(6).
           05  SR-DATA                 PIC X(130).
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY XMDBATWK.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARMGET==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXCPRAIS==.
       01  WS-LEG-NAME                 PIC X(200) VALUE SPACES.
       01  WS-LEG-STATUS               PIC X(2) VALUE "00".
       01  WS-FILE-EOF                 PIC X VALUE "N".
       01  WS-BATCH.
           05  WS-BATCH-REF            PIC X(40) VALUE SPACES.
           05  WS-BATCH-ID             PIC 9(9) VALUE 0.
           05  WS-CUTOVER              PIC 9(8) VALUE 0.
           05  WS-IMPORT-TYPE          PIC X(30) VALUE SPACES.
           05  WS-BASENAME             PIC X(30) VALUE SPACES.
      *>   COM-015.  One conversion stock line is one idempotent
      *>   instruction.  The row sequence used to sit at the end of a
      *>   correlation id that was then cut to the 36 characters the
      *>   commarea holds, so a long file name truncated the sequence
      *>   away and every line of the load shared a single key - only
      *>   the first line was ever posted (CR2741).
           05  WS-CORR-STEM            PIC X(26) VALUE SPACES.
           05  WS-PREV-KEY             PIC X(64) VALUE SPACES.
           05  WS-PCNT                 PIC S9(4) VALUE 2.
           05  WS-SEQ                  PIC 9(6) VALUE 0.
      *>   the record as it arrives, and the four type overlays
       01  WS-IN                       PIC X(130) VALUE SPACES.
       01  WS-IN-COM REDEFINES WS-IN.
           05  IN-TYPE                 PIC X(4).
           05  FILLER                  PIC X(126).
       01  WS-AR-REC.
           05  AR-PARTY                PIC X(10).
           05  AR-DOCNO                PIC X(10).
           05  AR-KIND                 PIC X(2).
           05  AR-DOCDATE              PIC X(8).
           05  AR-DUEDATE              PIC X(8).
           05  AR-CCY                  PIC X(3).
           05  AR-AMT                  PIC X(15).
           05  AR-TERMS                PIC X(10).
           05  AR-REF                  PIC X(10).
       01  WS-ST-REC.
           05  ST-SITE                 PIC X(10).
           05  ST-WH                   PIC X(10).
           05  ST-ITEM                 PIC X(40).
           05  ST-REV                  PIC X(5).
           05  ST-LOT                  PIC X(10).
           05  ST-QTY                  PIC X(15).
           05  ST-COST                 PIC X(15).
           05  ST-LOC                  PIC X(10).
       01  WS-GL-REC.
           05  GL-ACCT                 PIC X(12).
           05  GL-DATE                 PIC X(8).
           05  GL-DR                   PIC X(15).
           05  GL-CR                   PIC X(15).
           05  GL-NARR                 PIC X(30).
           05  GL-CC                   PIC X(10).
       01  WS-DET.
           05  D-SEQ                   PIC 9(6) VALUE 0.
           05  D-TYPE                  PIC X(4) VALUE SPACES.
           05  D-KEY                   PIC X(44) VALUE SPACES.
           05  D-PARTY                 PIC X(12) VALUE SPACES.
           05  D-DATE                  PIC X(10) VALUE SPACES.
           05  D-CCY                   PIC X(3) VALUE SPACES.
           05  D-VALUE                 PIC S9(13)V99 VALUE 0.
           05  D-QTY                   PIC S9(11)V9(3) VALUE 0.
           05  D-OUTCOME               PIC X(10) VALUE SPACES.
           05  D-REASON                PIC X(52) VALUE SPACES.
       01  WS-TOT.
           05  T-ACCEPT-VAL            PIC S9(13)V99 VALUE 0.
           05  T-REJECT-VAL            PIC S9(13)V99 VALUE 0.
           05  T-DR                    PIC S9(13)V99 VALUE 0.
           05  T-CR                    PIC S9(13)V99 VALUE 0.
           05  T-PROOF                 PIC S9(13)V99 VALUE 0.
       01  WS-SCOPE.
           05  S-FILE                  PIC X(60) VALUE SPACES.
           05  S-CUT                   PIC X(10) VALUE SPACES.
           05  S-MODE                  PIC X(10) VALUE SPACES.
           05  S-BATCH                 PIC X(40) VALUE SPACES.
      *>   what the accepted record became in the live ledger, kept
      *>   on the staging row so the conversion can be audited
       01  WS-CREATED.
           05  WS-CRE-TYPE             PIC X(20) VALUE SPACES.
           05  WS-CRE-REF              PIC X(40) VALUE SPACES.
           05  WS-TXN-OPEN             PIC X VALUE "N".
           05  WS-POST-OK              PIC X VALUE "Y".
       COPY STKMOVECA REPLACING ==DFHCOMMAREA== BY ==WS-SM==.
       REPORT SECTION.
       RD  R-CONVLOAD
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 8 LAST DETAIL 52
           FOOTING 58.
       COPY STRPTHDR.
       01  DE-SCOPE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(12) VALUE "Source file ".
               10  COLUMN 13   PIC X(60) SOURCE S-FILE.
               10  COLUMN 76   PIC X(11) VALUE "Cut over   ".
               10  COLUMN 87   PIC X(10) SOURCE S-CUT.
               10  COLUMN 100  PIC X(6) VALUE "Mode  ".
               10  COLUMN 106  PIC X(10) SOURCE S-MODE.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(12) VALUE "Batch ref   ".
               10  COLUMN 13   PIC X(40) SOURCE S-BATCH.
               10  COLUMN 56   PIC X(70) VALUE
                   "Duplicate tests: in file, in ledger, in batch.".
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(3) VALUE "Seq".
               10  COLUMN 9    PIC X(4) VALUE "Type".
               10  COLUMN 15   PIC X(3) VALUE "Key".
               10  COLUMN 61   PIC X(5) VALUE "Party".
               10  COLUMN 75   PIC X(4) VALUE "Date".
               10  COLUMN 87   PIC X(3) VALUE "Ccy".
               10  COLUMN 94   PIC X(12) VALUE "Value / qty ".
               10  COLUMN 110  PIC X(7) VALUE "Outcome".
               10  COLUMN 121  PIC X(6) VALUE "Reason".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-ROW TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC ZZZZZ9 SOURCE D-SEQ.
               10  COLUMN 9    PIC X(4) SOURCE D-TYPE.
               10  COLUMN 15   PIC X(44) SOURCE D-KEY.
               10  COLUMN 61   PIC X(12) SOURCE D-PARTY.
               10  COLUMN 75   PIC X(10) SOURCE D-DATE.
               10  COLUMN 87   PIC X(3) SOURCE D-CCY.
               10  COLUMN 92   PIC ZZZ,ZZZ,ZZ9.99- SOURCE D-VALUE.
               10  COLUMN 110  PIC X(10) SOURCE D-OUTCOME.
               10  COLUMN 121  PIC X(12) SOURCE D-REASON.
       01  DE-WHY TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 17   PIC X(10) VALUE "Reason:   ".
               10  COLUMN 27   PIC X(52) SOURCE D-REASON.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(12) VALUE "Records read".
               10  COLUMN 14   PIC Z(6)9 SOURCE XB-ROWS-IN.
               10  COLUMN 24   PIC X(9) VALUE "Accepted ".
               10  COLUMN 33   PIC Z(6)9 SOURCE XB-ACCEPTED.
               10  COLUMN 43   PIC X(9) VALUE "Rejected ".
               10  COLUMN 52   PIC Z(6)9 SOURCE XB-REJECTED.
               10  COLUMN 62   PIC X(11) VALUE "Duplicates ".
               10  COLUMN 73   PIC Z(6)9 SOURCE XB-DUPLICATE.
               10  COLUMN 83   PIC X(18) VALUE "Already loaded    ".
               10  COLUMN 101  PIC Z(6)9 SOURCE XB-SKIPPED.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(16) VALUE "Accepted value  ".
               10  COLUMN 18   PIC ZZZ,ZZZ,ZZ9.99- SOURCE
                                   T-ACCEPT-VAL.
               10  COLUMN 38   PIC X(16) VALUE "Rejected value  ".
               10  COLUMN 55   PIC ZZZ,ZZZ,ZZ9.99- SOURCE
                                   T-REJECT-VAL.
               10  COLUMN 76   PIC X(10) VALUE "Debits    ".
               10  COLUMN 86   PIC ZZZ,ZZZ,ZZ9.99 SOURCE T-DR.
               10  COLUMN 103  PIC X(8) VALUE "Credits ".
               10  COLUMN 112  PIC ZZZ,ZZZ,ZZ9.99 SOURCE T-CR.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(24) VALUE
                   "Trial balance proof     ".
               10  COLUMN 26   PIC ZZZ,ZZZ,ZZ9.99- SOURCE T-PROOF.
               10  COLUMN 48   PIC X(20) SOURCE RPT-STATUS-TEXT.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(64) VALUE
                   "A rejected record is not loaded and is not".
               10  COLUMN 48   PIC X(62) VALUE
                   "counted in the totals. Correct and re-run.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF XB-RC > 4
               GO TO 0000-FINISH
           END-IF
           MOVE "CONVLOAD" TO RPT-REPORT-ID
           MOVE "Legacy conversion load" TO RPT-TITLE
           MOVE "Per record validation, duplicate control and load"
               TO RPT-SUBTITLE
           PERFORM 9100-JOB-START THRU 9100-EXIT
           PERFORM 9200-OPEN-SPOOL THRU 9200-EXIT
           IF XB-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0300-OPEN-BATCH THRU 0300-EXIT
           PERFORM 0400-BEGIN-WORK THRU 0400-EXIT
           INITIATE R-CONVLOAD
           GENERATE DE-SCOPE
           SORT SORT-FILE
               ON ASCENDING KEY SR-TYPE SR-KEY SR-SEQ
               INPUT PROCEDURE IS 1000-READ-INPUT
                   THRU 1000-INPUT-EXIT
               OUTPUT PROCEDURE IS 2000-PROCESS
                   THRU 2000-PROCESS-EXIT
           PERFORM 3000-PROVE THRU 3000-EXIT
           PERFORM 3400-END-WORK THRU 3400-EXIT
           TERMINATE R-CONVLOAD
           PERFORM 9350-CLOSE-SPOOL THRU 9350-EXIT
           PERFORM 3500-CLOSE-BATCH THRU 3500-EXIT.
       0000-FINISH.
           PERFORM 9150-JOB-END THRU 9150-EXIT
           MOVE XB-RC TO RETURN-CODE
           STOP RUN.

       0100-READ-ARGS.
           PERFORM 9500-STD-ARGS THRU 9500-EXIT
           MOVE "FILE" TO XB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           IF XB-TEXT = SPACES
               MOVE "data\legacy\CALAROPN.DAT" TO WS-LEG-NAME
           ELSE
               MOVE XB-TEXT TO WS-LEG-NAME
           END-IF
           MOVE "CUTOVER" TO XB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           IF XB-TEXT NOT = SPACES
               COMPUTE WS-CUTOVER = FUNCTION NUMVAL(XB-TEXT)
           ELSE
               MOVE XB-ASAT TO WS-CUTOVER
           END-IF
           PERFORM 0150-BASENAME THRU 0150-EXIT
           MOVE SPACES TO WS-BATCH-REF
           MOVE WS-CUTOVER TO XB-DATE-ED
           STRING "CONV-" FUNCTION TRIM(WS-BASENAME) "-" XB-DATE-ED
               DELIMITED BY SIZE INTO WS-BATCH-REF
           MOVE WS-BATCH-REF TO S-BATCH
           MOVE WS-LEG-NAME(1:60) TO S-FILE
           MOVE WS-CUTOVER TO XB-NUM-DATE
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO S-CUT
           MOVE XB-MODE TO S-MODE
           MOVE "LEGACY" TO WS-IMPORT-TYPE
           DISPLAY "STERP-I-0100 CONVLOAD " FUNCTION TRIM(XB-CO)
                   " " FUNCTION TRIM(WS-LEG-NAME)
                   " mode " FUNCTION TRIM(XB-MODE).
       0100-EXIT.
           EXIT.

       0150-BASENAME.
      *>   last path element without its extension - the 1993 routine
      *>   scanned backwards for the separator and has never changed
           MOVE SPACES TO WS-BASENAME
           MOVE 0 TO XB-J
           PERFORM VARYING XB-I FROM 200 BY -1 UNTIL XB-I < 1
               IF XB-J = 0
                   IF WS-LEG-NAME(XB-I:1) = "\"
                       OR WS-LEG-NAME(XB-I:1) = "/"
                       COMPUTE XB-J = XB-I + 1
                   END-IF
               END-IF
           END-PERFORM
           IF XB-J = 0
               MOVE 1 TO XB-J
           END-IF
           MOVE WS-LEG-NAME(XB-J:30) TO WS-BASENAME
           PERFORM VARYING XB-I FROM 1 BY 1 UNTIL XB-I > 30
               IF WS-BASENAME(XB-I:1) = "."
                   MOVE SPACES TO WS-BASENAME(XB-I:31 - XB-I)
               END-IF
           END-PERFORM.
       0150-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: COM-023, XMD-007  the batch reference is derived, so a
      *> restart finds the batch it started and resumes it
      *>----------------------------------------------------------------
       0300-OPEN-BATCH.
           IF NOT XB-COMMIT
               GO TO 0300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "MERGE stg.ImportBatch AS t USING (SELECT ? AS "
                  "CompanyCode, ? AS BatchRef) AS s ON "
                  "t.CompanyCode = s.CompanyCode AND t.BatchRef = "
                  "s.BatchRef WHEN MATCHED THEN UPDATE SET Status = "
                  "'OPEN', Mode = 'COMMIT' WHEN NOT MATCHED THEN "
                  "INSERT (CompanyCode, BatchRef, ImportType, "
                  "SourceFile, SubmittedBy, Mode, Status) VALUES (?"
                  ",?,?,?,?,'COMMIT','OPEN');"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1) SQLIO-PARM-VAL(3)
           MOVE WS-BATCH-REF TO SQLIO-PARM-VAL(2) SQLIO-PARM-VAL(4)
           MOVE WS-IMPORT-TYPE TO SQLIO-PARM-VAL(5)
           MOVE WS-LEG-NAME TO SQLIO-PARM-VAL(6)
           MOVE XB-USER TO SQLIO-PARM-VAL(7)
           PERFORM VARYING XB-J FROM 1 BY 1 UNTIL XB-J > 7
               MOVE "N" TO SQLIO-PARM-NULL(XB-J)
           END-PERFORM
           MOVE 7 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),BatchId) FROM "
                  "stg.ImportBatch WHERE CompanyCode = ? AND "
                  "BatchRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-BATCH-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-OK
               COMPUTE WS-BATCH-ID = FUNCTION NUMVAL(BR-COL(1))
           ELSE
               MOVE 12 TO XB-RC
               DISPLAY "STERP-E-0301 import batch not created"
           END-IF.
       0300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: XMD-008, COM-023  one unit of work for the whole record
      *> batch - a conversion is loaded completely or not at all
      *>----------------------------------------------------------------
       0400-BEGIN-WORK.
           MOVE "N" TO WS-TXN-OPEN
           IF NOT XB-COMMIT OR WS-BATCH-ID = 0 OR XB-RC > 4
               GO TO 0400-EXIT
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 8 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO XB-RC
               DISPLAY "STERP-E-0401 cannot start the load "
                       "transaction"
           ELSE
               MOVE "Y" TO WS-TXN-OPEN
           END-IF.
       0400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> SORT input phase - every line is released with its natural
      *> key in front so that duplicates land next to one another
      *>----------------------------------------------------------------
       1000-READ-INPUT.
           OPEN INPUT LEG-FILE
           IF WS-LEG-STATUS NOT = "00"
               MOVE 8 TO XB-RC
               DISPLAY "STERP-E-1001 cannot open "
                       FUNCTION TRIM(WS-LEG-NAME)
                       " status " WS-LEG-STATUS
               GO TO 1000-INPUT-EXIT
           END-IF
           MOVE "N" TO WS-FILE-EOF
           PERFORM UNTIL WS-FILE-EOF = "Y"
               MOVE SPACES TO LEG-REC
               READ LEG-FILE
                   AT END MOVE "Y" TO WS-FILE-EOF
               END-READ
               IF WS-FILE-EOF = "N"
                   IF LEG-REC NOT = SPACES
                       ADD 1 TO XB-ROWS-IN
                       ADD 1 TO WS-SEQ
                       MOVE SPACES TO SORT-REC
                       MOVE LEG-REC TO WS-IN
                       MOVE IN-TYPE TO SR-TYPE
                       MOVE WS-SEQ TO SR-SEQ
                       MOVE LEG-REC TO SR-DATA
                       PERFORM 1100-NATURAL-KEY THRU 1100-EXIT
                       RELEASE SORT-REC
                   END-IF
               END-IF
           END-PERFORM
           CLOSE LEG-FILE.
       1000-INPUT-EXIT.
           EXIT.

       1100-NATURAL-KEY.
           MOVE SPACES TO SR-KEY
           EVALUATE IN-TYPE
               WHEN "AROP"
                   MOVE WS-IN(5:20) TO SR-KEY
               WHEN "APOP"
                   MOVE WS-IN(5:20) TO SR-KEY
               WHEN "STKB"
                   MOVE WS-IN(5:20) TO SR-KEY(1:20)
                   MOVE WS-IN(25:40) TO SR-KEY(21:40)
               WHEN "GLTB"
                   MOVE WS-IN(5:20) TO SR-KEY
               WHEN OTHER
                   MOVE WS-IN(5:20) TO SR-KEY
           END-EVALUATE.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> SORT output phase - validate, decide the outcome and record it
      *>----------------------------------------------------------------
       2000-PROCESS.
           MOVE SPACES TO WS-PREV-KEY
           MOVE "N" TO XB-DONE
           PERFORM UNTIL XB-FINISHED
               RETURN SORT-FILE
                   AT END MOVE "Y" TO XB-DONE
               END-RETURN
               IF NOT XB-FINISHED
                   PERFORM 2100-ONE-RECORD THRU 2100-EXIT
               END-IF
           END-PERFORM.
       2000-PROCESS-EXIT.
           EXIT.

       2100-ONE-RECORD.
           MOVE SR-DATA TO WS-IN
           MOVE SR-SEQ TO D-SEQ
           MOVE SR-TYPE TO D-TYPE
           MOVE SR-KEY(1:44) TO D-KEY
           MOVE SPACES TO D-PARTY
           MOVE SPACES TO D-DATE
           MOVE SPACES TO D-CCY
           MOVE 0 TO D-VALUE
           MOVE 0 TO D-QTY
           MOVE "ACCEPTED" TO D-OUTCOME
           MOVE SPACES TO D-REASON
           MOVE 2 TO WS-PCNT
           MOVE SR-TYPE TO WS-CRE-TYPE
           MOVE SR-KEY(1:40) TO WS-CRE-REF
           EVALUATE SR-TYPE
               WHEN "AROP" PERFORM 2200-VALID-AR THRU 2200-EXIT
               WHEN "APOP" PERFORM 2300-VALID-AP THRU 2300-EXIT
               WHEN "STKB" PERFORM 2400-VALID-ST THRU 2400-EXIT
               WHEN "GLTB" PERFORM 2500-VALID-GL THRU 2500-EXIT
               WHEN OTHER
                   MOVE "REJECTED" TO D-OUTCOME
                   MOVE "Unknown record type in the extract"
                       TO D-REASON
           END-EVALUATE
           IF D-OUTCOME = "ACCEPTED"
               IF SR-TYPE = WS-PREV-KEY(1:4)
                   AND SR-KEY(1:60) = WS-PREV-KEY(5:60)
                   MOVE "DUPLICATE" TO D-OUTCOME
                   MOVE "The same key appears earlier in the file"
                       TO D-REASON
               END-IF
           END-IF
           MOVE SR-TYPE TO WS-PREV-KEY(1:4)
           MOVE SR-KEY(1:60) TO WS-PREV-KEY(5:60)
           IF D-OUTCOME = "ACCEPTED"
               PERFORM 2700-ALREADY-LOADED THRU 2700-EXIT
           END-IF
           IF D-OUTCOME = "ACCEPTED"
               PERFORM 2600-LEDGER-DUP THRU 2600-EXIT
           END-IF
           IF D-OUTCOME = "ACCEPTED"
               PERFORM 2850-POST-RECORD THRU 2850-EXIT
           END-IF
           PERFORM 2800-COUNT THRU 2800-EXIT
           GENERATE DE-ROW
           IF D-OUTCOME NOT = "ACCEPTED" AND D-REASON NOT = SPACES
               GENERATE DE-WHY
           END-IF
           PERFORM 2900-WRITE-ROW THRU 2900-EXIT.
       2100-EXIT.
           EXIT.

       2200-VALID-AR.
           MOVE WS-IN(5:80) TO WS-AR-REC
           MOVE AR-PARTY(1:10) TO D-PARTY
           MOVE AR-CCY TO D-CCY
           PERFORM 2250-COMMON-OPEN-ITEM THRU 2250-EXIT
           IF D-OUTCOME NOT = "ACCEPTED"
               GO TO 2200-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                  "ar.Customer WHERE CompanyCode = ? AND "
                  "CustomerCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE AR-PARTY TO SQLIO-PARM-VAL(2)
           PERFORM 7000-COUNT-CHECK THRU 7000-EXIT
           IF XB-K = 0
               MOVE "REJECTED" TO D-OUTCOME
               MOVE "Customer is not on file at the cut over"
                   TO D-REASON
           END-IF.
       2200-EXIT.
           EXIT.

       2300-VALID-AP.
           MOVE WS-IN(5:80) TO WS-AR-REC
           MOVE AR-PARTY(1:10) TO D-PARTY
           MOVE AR-CCY TO D-CCY
           PERFORM 2250-COMMON-OPEN-ITEM THRU 2250-EXIT
           IF D-OUTCOME NOT = "ACCEPTED"
               GO TO 2300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                  "ap.Supplier WHERE CompanyCode = ? AND "
                  "SupplierCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE AR-PARTY TO SQLIO-PARM-VAL(2)
           PERFORM 7000-COUNT-CHECK THRU 7000-EXIT
           IF XB-K = 0
               MOVE "REJECTED" TO D-OUTCOME
               MOVE "Supplier is not on file at the cut over"
                   TO D-REASON
           END-IF.
       2300-EXIT.
           EXIT.

       2250-COMMON-OPEN-ITEM.
           IF AR-DOCNO = SPACES
               MOVE "REJECTED" TO D-OUTCOME
               MOVE "Document number is blank" TO D-REASON
               GO TO 2250-EXIT
           END-IF
           MOVE AR-DOCDATE TO XB-TEXT
           PERFORM 7100-CHECK-DATE THRU 7100-EXIT
           IF XB-K = 0
               MOVE "REJECTED" TO D-OUTCOME
               MOVE "Document date is not a real calendar date"
                   TO D-REASON
               GO TO 2250-EXIT
           END-IF
           MOVE XB-NUM-DATE TO XB-NUM-DATE
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO D-DATE
           IF XB-NUM-DATE > WS-CUTOVER
               MOVE "REJECTED" TO D-OUTCOME
               MOVE "Document date is after the cut over date"
                   TO D-REASON
               GO TO 2250-EXIT
           END-IF
           MOVE AR-DUEDATE TO XB-TEXT
           PERFORM 7100-CHECK-DATE THRU 7100-EXIT
           IF XB-K = 0
               MOVE "REJECTED" TO D-OUTCOME
               MOVE "Due date is missing or not a calendar date"
                   TO D-REASON
               GO TO 2250-EXIT
           END-IF
           MOVE AR-AMT TO XB-TEXT
           PERFORM 7200-CHECK-NUM THRU 7200-EXIT
           IF XB-K = 0
               MOVE "REJECTED" TO D-OUTCOME
               MOVE "Amount is not numeric in the extract"
                   TO D-REASON
               GO TO 2250-EXIT
           END-IF
           MOVE XB-AMT TO D-VALUE
           IF D-VALUE = 0
               MOVE "REJECTED" TO D-OUTCOME
               MOVE "Open item of nil value is not converted"
                   TO D-REASON
           END-IF.
       2250-EXIT.
           EXIT.

       2400-VALID-ST.
           MOVE WS-IN(5:115) TO WS-ST-REC
           MOVE ST-SITE(1:10) TO D-PARTY
           MOVE ST-QTY TO XB-TEXT
           PERFORM 7200-CHECK-NUM THRU 7200-EXIT
           IF XB-K = 0
               MOVE "REJECTED" TO D-OUTCOME
               MOVE "Quantity is not numeric in the extract"
                   TO D-REASON
               GO TO 2400-EXIT
           END-IF
           MOVE XB-AMT TO D-QTY
           IF D-QTY < 0
               MOVE "REJECTED" TO D-OUTCOME
               MOVE "Negative opening stock is refused (D-09)"
                   TO D-REASON
               GO TO 2400-EXIT
           END-IF
           MOVE ST-COST TO XB-TEXT
           PERFORM 7200-CHECK-NUM THRU 7200-EXIT
           IF XB-K = 0
               MOVE "REJECTED" TO D-OUTCOME
               MOVE "Unit cost is not numeric in the extract"
                   TO D-REASON
               GO TO 2400-EXIT
           END-IF
           COMPUTE D-VALUE ROUNDED = D-QTY * XB-AMT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                  "inv.Item WHERE CompanyCode = ? AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE ST-ITEM TO SQLIO-PARM-VAL(2)
           PERFORM 7000-COUNT-CHECK THRU 7000-EXIT
           IF XB-K = 0
               MOVE "REJECTED" TO D-OUTCOME
               MOVE "Item is not on file at the cut over"
                   TO D-REASON
               GO TO 2400-EXIT
           END-IF
      *>   the movement is dated at the cut over but booked in the
      *>   period the conversion is run in; an unusable period is
      *>   refused here, because the posting contract raises and
      *>   that would abandon the whole batch transaction
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                  "fin.Period WHERE CompanyCode = ? AND "
                  "CONVERT(date,?,112) BETWEEN StartDate AND "
                  "EndDate AND Status IN ('OPEN','SOFT')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-ASAT TO XB-DATE-ED
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(2)
           MOVE 2 TO WS-PCNT
           PERFORM 7000-COUNT-CHECK THRU 7000-EXIT
           IF XB-K = 0
               MOVE "REJECTED" TO D-OUTCOME
               MOVE "No open period to book the opening stock in"
                   TO D-REASON
           END-IF.
       2400-EXIT.
           EXIT.

       2500-VALID-GL.
           MOVE WS-IN(5:90) TO WS-GL-REC
           MOVE GL-CC(1:10) TO D-PARTY
           MOVE GL-DATE TO XB-TEXT
           PERFORM 7100-CHECK-DATE THRU 7100-EXIT
           IF XB-K = 0
               MOVE "REJECTED" TO D-OUTCOME
               MOVE "Balance date is not a real calendar date"
                   TO D-REASON
               GO TO 2500-EXIT
           END-IF
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO D-DATE
           MOVE GL-DR TO XB-TEXT
           PERFORM 7200-CHECK-NUM THRU 7200-EXIT
           IF XB-K = 0
               MOVE "REJECTED" TO D-OUTCOME
               MOVE "Debit amount is not numeric" TO D-REASON
               GO TO 2500-EXIT
           END-IF
           MOVE XB-AMT TO XB-AMT2
           MOVE GL-CR TO XB-TEXT
           PERFORM 7200-CHECK-NUM THRU 7200-EXIT
           IF XB-K = 0
               MOVE "REJECTED" TO D-OUTCOME
               MOVE "Credit amount is not numeric" TO D-REASON
               GO TO 2500-EXIT
           END-IF
           COMPUTE D-VALUE = XB-AMT2 - XB-AMT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                  "fin.Account WHERE CompanyCode = ? AND "
                  "AccountCode = ? AND AccountKind <> 'SUMMARY'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE GL-ACCT TO SQLIO-PARM-VAL(2)
           PERFORM 7000-COUNT-CHECK THRU 7000-EXIT
           IF XB-K = 0
               MOVE "REJECTED" TO D-OUTCOME
               MOVE "Account is not a posting account on file"
                   TO D-REASON
           END-IF.
       2500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: XMD-007, COM-023  a source instruction processed twice
      *> cannot create two open items
      *>----------------------------------------------------------------
       2600-LEDGER-DUP.
           MOVE SPACES TO SQLIO-STMT
           MOVE 2 TO WS-PCNT
           EVALUATE SR-TYPE
               WHEN "AROP"
                   STRING "SELECT CONVERT(varchar(12),COUNT(*)) "
                          "FROM ar.OpenItem WHERE CompanyCode = ? "
                          "AND DocumentNumber = ?"
                       DELIMITED BY SIZE INTO SQLIO-STMT
                   MOVE AR-DOCNO TO SQLIO-PARM-VAL(2)
               WHEN "APOP"
                   STRING "SELECT CONVERT(varchar(12),COUNT(*)) "
                          "FROM ap.OpenItem WHERE CompanyCode = ? "
                          "AND DocumentNumber = ?"
                       DELIMITED BY SIZE INTO SQLIO-STMT
                   MOVE AR-DOCNO TO SQLIO-PARM-VAL(2)
               WHEN "GLTB"
                   STRING "SELECT CONVERT(varchar(12),COUNT(*)) "
                          "FROM fin.OpeningBalance WHERE "
                          "CompanyCode = ? AND AccountCode = ? AND "
                          "ISNULL(BatchRef,'') <> ?"
                       DELIMITED BY SIZE INTO SQLIO-STMT
                   MOVE GL-ACCT TO SQLIO-PARM-VAL(2)
                   MOVE WS-BATCH-REF TO SQLIO-PARM-VAL(3)
                   MOVE 3 TO WS-PCNT
               WHEN "STKB"
                   STRING "SELECT CONVERT(varchar(12),COUNT(*)) "
                          "FROM inv.StockMovement WHERE "
                          "CompanyCode = ? AND ItemCode = ? AND "
                          "MovementType = 'OPENING' AND "
                          "LocationCode = ? AND ISNULL(LotCode,'')"
                          " = ?"
                       DELIMITED BY SIZE INTO SQLIO-STMT
                   MOVE ST-ITEM TO SQLIO-PARM-VAL(2)
                   MOVE ST-LOC TO SQLIO-PARM-VAL(3)
                   MOVE ST-LOT TO SQLIO-PARM-VAL(4)
                   MOVE 4 TO WS-PCNT
               WHEN OTHER
                   GO TO 2600-EXIT
           END-EVALUATE
           PERFORM 7000-COUNT-CHECK THRU 7000-EXIT
           IF XB-K > 0
               MOVE "DUPLICATE" TO D-OUTCOME
               MOVE "The ledger already holds this reference"
                   TO D-REASON
           END-IF.
       2600-EXIT.
           EXIT.

       2700-ALREADY-LOADED.
           IF WS-BATCH-ID = 0
               GO TO 2700-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                  "stg.ImportRow WHERE BatchId = ? AND NaturalKey "
                  "= ? AND ValidationOutcome = 'VALID' AND "
                  "ProcessedAt IS NOT NULL"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-BATCH-ID TO XB-SEQ-ED
           MOVE FUNCTION TRIM(XB-SEQ-ED) TO SQLIO-PARM-VAL(1)
           MOVE SPACES TO XB-TEXT
           STRING SR-TYPE "|" SR-KEY(1:60)
               DELIMITED BY SIZE INTO XB-TEXT
           MOVE XB-TEXT(1:120) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           MOVE 0 TO XB-K
           IF SQLIO-OK
               COMPUTE XB-K = FUNCTION NUMVAL(BR-COL(1))
           END-IF
           IF XB-K > 0
               MOVE "LOADED" TO D-OUTCOME
               MOVE "Loaded by an earlier run of this batch"
                   TO D-REASON
           END-IF.
       2700-EXIT.
           EXIT.

       2800-COUNT.
           EVALUATE D-OUTCOME
               WHEN "ACCEPTED"
                   ADD 1 TO XB-ACCEPTED
                   ADD D-VALUE TO T-ACCEPT-VAL
                   IF SR-TYPE = "GLTB"
                       IF D-VALUE >= 0
                           ADD D-VALUE TO T-DR
                       ELSE
                           COMPUTE T-CR = T-CR - D-VALUE
                       END-IF
                   END-IF
               WHEN "DUPLICATE"
                   ADD 1 TO XB-DUPLICATE
                   ADD D-VALUE TO T-REJECT-VAL
               WHEN "LOADED"
                   ADD 1 TO XB-SKIPPED
               WHEN OTHER
                   ADD 1 TO XB-REJECTED
                   ADD D-VALUE TO T-REJECT-VAL
           END-EVALUATE
           ADD 1 TO XB-ROWS-OUT.
       2800-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: XMD-007, AR-021, AP-024, ITM-024, FIN-024  an accepted
      *> record becomes a live ledger item: a debtor or creditor open
      *> item, an opening stock movement or an opening balance.  All
      *> of it sits inside the batch transaction opened by 0400.
      *>----------------------------------------------------------------
       2850-POST-RECORD.
           MOVE SR-TYPE TO WS-CRE-TYPE
           MOVE SR-KEY(1:40) TO WS-CRE-REF
           MOVE "Y" TO WS-POST-OK
           IF NOT XB-COMMIT OR WS-BATCH-ID = 0
               GO TO 2850-EXIT
           END-IF
           EVALUATE SR-TYPE
               WHEN "AROP" PERFORM 2860-OPEN-ITEM THRU 2860-EXIT
               WHEN "APOP" PERFORM 2860-OPEN-ITEM THRU 2860-EXIT
               WHEN "STKB" PERFORM 2880-STOCK THRU 2880-EXIT
               WHEN "GLTB"
                   MOVE "OPENINGBAL" TO WS-CRE-TYPE
                   MOVE GL-ACCT TO WS-CRE-REF
                   PERFORM 2950-OPENING-BALANCE
                               THRU 2950-EXIT
               WHEN OTHER CONTINUE
           END-EVALUATE.
       2850-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> the open item is written at the converted residual - the
      *> legacy extract carries no settlement history, so the whole
      *> balance is outstanding at the cut over
      *>----------------------------------------------------------------
       2860-OPEN-ITEM.
           MOVE SPACES TO SQLIO-STMT
           IF AR-KIND = "CR"
               MOVE "CREDIT" TO XB-TEXT2
           ELSE
               MOVE "INVOICE" TO XB-TEXT2
           END-IF
           IF SR-TYPE = "AROP"
               STRING "DECLARE @a decimal(18,2) = ?, @c char(3) = "
                      "?, @d date = CONVERT(date,?,112); DECLARE "
                      "@r decimal(18,8) = ISNULL((SELECT TOP 1 "
                      "f.UnitsPerGBP FROM core.FxRate f WHERE "
                      "f.CurrencyCode = @c AND f.RateDate <= @d "
                      "ORDER BY f.RateDate DESC),1); INSERT INTO "
                      "ar.OpenItem (CompanyCode, CustomerCode, "
                      "ItemType, DocumentNumber, DocumentDate, "
                      "DueDate, PostingDate, CurrencyCode, FxRate,"
                      " OriginalAmount, OriginalAmountGBP, "
                      "ResidualAmount, ResidualAmountGBP, "
                      "TermsCode, SettlementStatus, CreatedBy) "
                      "SELECT ?,?,?,?,CONVERT(date,?,112),"
                      "CONVERT(date,?,112),@d,@c,@r,@a,"
                      "ROUND(@a/@r,2),@a,ROUND(@a/@r,2),"
                      "NULLIF(?,''),'OPEN',? WHERE NOT EXISTS "
                      "(SELECT 1 FROM ar.OpenItem o WHERE "
                      "o.CompanyCode = ? AND o.DocumentNumber = ? "
                      "AND o.ItemType = ?);"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE "AROPENITEM" TO WS-CRE-TYPE
           ELSE
               STRING "DECLARE @a decimal(18,2) = ?, @c char(3) = "
                      "?, @d date = CONVERT(date,?,112); DECLARE "
                      "@r decimal(18,8) = ISNULL((SELECT TOP 1 "
                      "f.UnitsPerGBP FROM core.FxRate f WHERE "
                      "f.CurrencyCode = @c AND f.RateDate <= @d "
                      "ORDER BY f.RateDate DESC),1); INSERT INTO "
                      "ap.OpenItem (CompanyCode, SupplierCode, "
                      "ItemType, DocumentNumber, SupplierDocRef, "
                      "DocumentDate, DueDate, PostingDate, "
                      "CurrencyCode, FxRate, OriginalAmount, "
                      "OriginalAmountGBP, ResidualAmount, "
                      "ResidualAmountGBP, TermsCode, "
                      "SettlementStatus, CreatedBy) SELECT ?,?,?,?"
                      ",NULLIF(?,''),CONVERT(date,?,112),"
                      "CONVERT(date,?,112),@d,@c,@r,@a,"
                      "ROUND(@a/@r,2),@a,ROUND(@a/@r,2),"
                      "NULLIF(?,''),'OPEN',? WHERE NOT EXISTS "
                      "(SELECT 1 FROM ap.OpenItem o WHERE "
                      "o.CompanyCode = ? AND o.DocumentNumber = ? "
                      "AND o.ItemType = ?);"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE "APOPENITEM" TO WS-CRE-TYPE
           END-IF
           MOVE D-VALUE TO XB-MON-ED
           MOVE FUNCTION TRIM(XB-MON-ED) TO SQLIO-PARM-VAL(1)
           MOVE AR-CCY TO SQLIO-PARM-VAL(2)
           MOVE WS-CUTOVER TO XB-DATE-ED
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(3)
           MOVE XB-CO TO SQLIO-PARM-VAL(4)
           MOVE AR-PARTY TO SQLIO-PARM-VAL(5)
           MOVE XB-TEXT2(1:12) TO SQLIO-PARM-VAL(6)
           MOVE AR-DOCNO TO SQLIO-PARM-VAL(7)
           IF SR-TYPE = "AROP"
               MOVE AR-DOCDATE TO SQLIO-PARM-VAL(8)
               MOVE AR-DUEDATE TO SQLIO-PARM-VAL(9)
               MOVE AR-TERMS TO SQLIO-PARM-VAL(10)
               MOVE XB-USER TO SQLIO-PARM-VAL(11)
               MOVE XB-CO TO SQLIO-PARM-VAL(12)
               MOVE AR-DOCNO TO SQLIO-PARM-VAL(13)
               MOVE XB-TEXT2(1:12) TO SQLIO-PARM-VAL(14)
               MOVE 14 TO SQLIO-PARM-CNT
           ELSE
               MOVE AR-REF TO SQLIO-PARM-VAL(8)
               MOVE AR-DOCDATE TO SQLIO-PARM-VAL(9)
               MOVE AR-DUEDATE TO SQLIO-PARM-VAL(10)
               MOVE AR-TERMS TO SQLIO-PARM-VAL(11)
               MOVE XB-USER TO SQLIO-PARM-VAL(12)
               MOVE XB-CO TO SQLIO-PARM-VAL(13)
               MOVE AR-DOCNO TO SQLIO-PARM-VAL(14)
               MOVE XB-TEXT2(1:12) TO SQLIO-PARM-VAL(15)
               MOVE 15 TO SQLIO-PARM-CNT
           END-IF
           PERFORM VARYING XB-J FROM 1 BY 1 UNTIL XB-J > 15
               MOVE "N" TO SQLIO-PARM-NULL(XB-J)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE "REJECTED" TO D-OUTCOME
               MOVE "N" TO WS-POST-OK
               MOVE SPACES TO D-REASON
               STRING "The ledger refused the open item: "
                      SQLIO-ERRMSG(1:18)
                   DELIMITED BY SIZE INTO D-REASON
               MOVE 8 TO XB-RC
           ELSE
               MOVE AR-DOCNO TO WS-CRE-REF
               ADD 1 TO XB-POSTED
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       2860-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: ITM-024  opening stock goes through the shared contract
      *> inv.usp_PostStockMovement (MovementType OPENING) by way of
      *> STKMOVE - a balance is never written directly
      *>----------------------------------------------------------------
       2880-STOCK.
           MOVE ST-COST TO XB-TEXT
           PERFORM 7200-CHECK-NUM THRU 7200-EXIT
           MOVE WS-CUTOVER TO XB-DATE-ED
           INITIALIZE WS-SM
           MOVE XB-CO TO CA-COMPANY OF WS-SM
           MOVE ST-SITE TO CA-SITE OF WS-SM
           MOVE XB-USER TO CA-USER OF WS-SM
           MOVE WS-CUTOVER TO CA-BUSINESS-DATE OF WS-SM
           MOVE "OPENING" TO CA-FUNCTION OF WS-SM
           MOVE SPACES TO XB-TEXT2
           MOVE SR-SEQ TO XB-SEQ-ED
           STRING "CVL|" FUNCTION TRIM(WS-BASENAME) "|"
                  XB-DATE-ED
               DELIMITED BY SIZE INTO XB-TEXT2
           MOVE XB-TEXT2(1:26) TO WS-CORR-STEM
           MOVE SPACES TO CA-CORRELATION-ID OF WS-SM
           STRING FUNCTION TRIM(WS-CORR-STEM) "|"
                  FUNCTION TRIM(XB-SEQ-ED)
               DELIMITED BY SIZE INTO CA-CORRELATION-ID OF WS-SM
           MOVE ST-ITEM TO SM-ITEM-CODE
           MOVE ST-REV(1:4) TO SM-REVISION
           MOVE ST-SITE TO SM-SITE
           MOVE ST-WH TO SM-WAREHOUSE
           MOVE ST-LOC TO SM-LOCATION
           MOVE ST-LOT TO SM-LOT-CODE
           MOVE D-QTY TO SM-QUANTITY
           MOVE XB-AMT TO SM-UNIT-COST
           MOVE WS-CUTOVER TO SM-MOVEMENT-DATE
           MOVE XB-ASAT TO SM-POSTING-DATE
           MOVE "OPENING" TO SM-REASON-CODE
           MOVE "XMD" TO SM-SOURCE-MODULE
           MOVE "CONVERSION" TO SM-SOURCE-DOC-TYPE
           MOVE WS-BATCH-REF TO SM-SOURCE-DOC-REF
           MOVE SR-SEQ TO SM-SOURCE-DOC-LINE
           MOVE SPACES TO SM-REASON
           STRING "Legacy conversion load line "
                  FUNCTION TRIM(WS-BATCH-REF)
               DELIMITED BY SIZE INTO SM-REASON
           CALL "STKMOVE" USING WS-SM
           IF CA-RETURN-CODE OF WS-SM < 8
               MOVE "STOCKMOVE" TO WS-CRE-TYPE
               MOVE SM-MOVEMENT-ID TO XB-INT-ED
               MOVE FUNCTION TRIM(XB-INT-ED) TO WS-CRE-REF
               ADD 1 TO XB-POSTED
               IF SM-DUPLICATE = "Y"
                   MOVE "DUPLICATE" TO D-OUTCOME
                   MOVE "Stock already holds this opening balance"
                       TO D-REASON
               END-IF
           ELSE
               MOVE "REJECTED" TO D-OUTCOME
               MOVE "N" TO WS-POST-OK
               MOVE CA-MESSAGE OF WS-SM(1:52) TO D-REASON
               IF CA-RETURN-CODE OF WS-SM > 8
                   MOVE 12 TO XB-RC
               ELSE
                   IF XB-RC < 4
                       MOVE 4 TO XB-RC
                   END-IF
               END-IF
           END-IF.
       2880-EXIT.
           EXIT.

       2900-WRITE-ROW.
           IF NOT XB-COMMIT OR WS-BATCH-ID = 0
               GO TO 2900-EXIT
           END-IF
           IF D-OUTCOME = "LOADED"
               GO TO 2900-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "MERGE stg.ImportRow AS t USING (SELECT ? AS "
                  "BatchId, ? AS RowNum) AS s ON t.BatchId = "
                  "s.BatchId AND t.RowNum = s.RowNum WHEN MATCHED "
                  "THEN UPDATE SET NaturalKey = ?, RawData = ?, "
                  "ValidationOutcome = ?, ValidationMessage = ?, "
                  "CreatedDocumentType = ?, CreatedDocumentRef = ?,"
                  " ProcessedAt = SYSDATETIME(), ProcessedBy = ? "
                  "WHEN NOT MATCHED THEN INSERT (BatchId, RowNum, "
                  "NaturalKey, RawData, ValidationOutcome, "
                  "ValidationMessage, CreatedDocumentType, "
                  "CreatedDocumentRef, ProcessedAt, ProcessedBy) "
                  "VALUES (?,?,?,?,?,?,?,?,SYSDATETIME(),?);"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-BATCH-ID TO XB-SEQ-ED
           MOVE FUNCTION TRIM(XB-SEQ-ED) TO SQLIO-PARM-VAL(1)
                                            SQLIO-PARM-VAL(10)
           MOVE SR-SEQ TO XB-SEQ-ED
           MOVE FUNCTION TRIM(XB-SEQ-ED) TO SQLIO-PARM-VAL(2)
                                            SQLIO-PARM-VAL(11)
           MOVE SPACES TO XB-TEXT
           STRING SR-TYPE "|" SR-KEY(1:60)
               DELIMITED BY SIZE INTO XB-TEXT
           MOVE XB-TEXT(1:120) TO SQLIO-PARM-VAL(3)
                                  SQLIO-PARM-VAL(12)
           MOVE SR-DATA(1:130) TO SQLIO-PARM-VAL(4)
                                  SQLIO-PARM-VAL(13)
           EVALUATE D-OUTCOME
               WHEN "ACCEPTED"  MOVE "VALID" TO XB-TEXT2
               WHEN "DUPLICATE" MOVE "DUPLICATE" TO XB-TEXT2
               WHEN OTHER       MOVE "INVALID" TO XB-TEXT2
           END-EVALUATE
           MOVE XB-TEXT2(1:12) TO SQLIO-PARM-VAL(5)
                                  SQLIO-PARM-VAL(14)
           MOVE D-REASON TO SQLIO-PARM-VAL(6) SQLIO-PARM-VAL(15)
           MOVE WS-CRE-TYPE TO SQLIO-PARM-VAL(7) SQLIO-PARM-VAL(16)
           MOVE WS-CRE-REF TO SQLIO-PARM-VAL(8)
                              SQLIO-PARM-VAL(17)
           MOVE XB-USER TO SQLIO-PARM-VAL(9) SQLIO-PARM-VAL(18)
           PERFORM VARYING XB-J FROM 1 BY 1 UNTIL XB-J > 18
               MOVE "N" TO SQLIO-PARM-NULL(XB-J)
           END-PERFORM
           MOVE 18 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               DISPLAY "STERP-W-2901 " SQLIO-ERRMSG(1:90)
               MOVE 4 TO XB-RC
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       2900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-024  the opening trial balance is held as opening
      *> balances, never as an ordinary journal in a closed period
      *>----------------------------------------------------------------
       2950-OPENING-BALANCE.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO fin.OpeningBalance (CompanyCode, "
                  "BatchRef, AsAtDate, AccountCode, SourceType, "
                  "CurrencyCode, DebitAmountGBP, CreditAmountGBP, "
                  "DimCostCentre, Narrative, Status, LoadedBy) "
                  "SELECT ?,?,CONVERT(date,?,112),?,'LEDGER','GBP',"
                  "?,?,?,?,'LOADED',? WHERE NOT EXISTS (SELECT 1 "
                  "FROM fin.OpeningBalance WHERE CompanyCode = ? "
                  "AND BatchRef = ? AND AccountCode = ? AND "
                  "AsAtDate = CONVERT(date,?,112))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1) SQLIO-PARM-VAL(10)
           MOVE WS-BATCH-REF TO SQLIO-PARM-VAL(2)
                                SQLIO-PARM-VAL(11)
           MOVE WS-CUTOVER TO XB-DATE-ED
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(3) SQLIO-PARM-VAL(13)
           MOVE GL-ACCT TO SQLIO-PARM-VAL(4) SQLIO-PARM-VAL(12)
           IF D-VALUE >= 0
               MOVE D-VALUE TO XB-MON-ED
               MOVE FUNCTION TRIM(XB-MON-ED) TO SQLIO-PARM-VAL(5)
               MOVE "0.00" TO SQLIO-PARM-VAL(6)
           ELSE
               MOVE "0.00" TO SQLIO-PARM-VAL(5)
               COMPUTE XB-AMT2 = 0 - D-VALUE
               MOVE XB-AMT2 TO XB-MON-ED
               MOVE FUNCTION TRIM(XB-MON-ED) TO SQLIO-PARM-VAL(6)
           END-IF
           MOVE GL-CC TO SQLIO-PARM-VAL(7)
           MOVE GL-NARR TO SQLIO-PARM-VAL(8)
           MOVE XB-USER TO SQLIO-PARM-VAL(9)
           PERFORM VARYING XB-J FROM 1 BY 1 UNTIL XB-J > 13
               MOVE "N" TO SQLIO-PARM-NULL(XB-J)
           END-PERFORM
           MOVE 13 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               DISPLAY "STERP-W-2951 " SQLIO-ERRMSG(1:90)
               MOVE 4 TO XB-RC
           ELSE
               ADD 1 TO XB-POSTED
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       2950-EXIT.
           EXIT.

       3000-PROVE.
           COMPUTE T-PROOF = T-DR - T-CR
           IF T-DR = 0 AND T-CR = 0
               MOVE "NO TRIAL BALANCE" TO RPT-STATUS-TEXT
           ELSE
               IF T-PROOF > 0.004 OR T-PROOF < -0.004
                   MOVE "OUT OF BALANCE" TO RPT-STATUS-TEXT
                   MOVE 8 TO XB-RC
                   DISPLAY "STERP-E-3001 the accepted opening trial"
                           " balance does not balance"
                   PERFORM 3100-RAISE-UNBALANCED THRU 3100-EXIT
               ELSE
                   MOVE "IN BALANCE" TO RPT-STATUS-TEXT
               END-IF
           END-IF
           IF XB-REJECTED > 0 OR XB-DUPLICATE > 0
               IF XB-RC < 4
                   MOVE 4 TO XB-RC
               END-IF
           END-IF
           DISPLAY "STERP-I-3002 read " XB-ROWS-IN " accepted "
                   XB-ACCEPTED " rejected " XB-REJECTED
                   " duplicate " XB-DUPLICATE " already loaded "
                   XB-SKIPPED.
       3000-EXIT.
           EXIT.

       3100-RAISE-UNBALANCED.
           INITIALIZE WS-EXCPRAIS
           MOVE "ERROR" TO EX-SEVERITY
           MOVE "CONVERSION DIFFERENCE" TO EX-CATEGORY
           MOVE "IMPORTBATCH" TO EX-DOCUMENT-TYPE
           MOVE WS-BATCH-REF TO EX-DOCUMENT-REF
           MOVE T-PROOF TO XB-MON-ED
           MOVE SPACES TO XB-TEXT2
           STRING "The accepted opening trial balance of "
                  FUNCTION TRIM(WS-BATCH-REF)
                  " is out by GBP " FUNCTION TRIM(XB-MON-ED)
                  ". Correct the extract and re-run the batch; "
                  "rejected rows are not loaded."
               DELIMITED BY SIZE INTO XB-TEXT2
           MOVE XB-TEXT2(1:200) TO EX-MESSAGE-TEXT
           MOVE "FINCTRL" TO EX-OWNER-ROLE
           MOVE 1 TO EX-PRIORITY
           MOVE T-PROOF TO EX-AMOUNT-GBP
           MOVE WS-CUTOVER TO EX-DUE-AT
           PERFORM 9700-RAISE-EXCEPTION THRU 9700-EXIT.
       3100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: XMD-008  the batch transaction is committed only when
      *> the load is whole; anything worse than a warning rolls the
      *> whole conversion back and leaves the batch to be re-run
      *>----------------------------------------------------------------
       3400-END-WORK.
           IF WS-TXN-OPEN NOT = "Y"
               GO TO 3400-EXIT
           END-IF
           MOVE 8 TO SQLIO-CURSOR
           IF XB-RC > 4
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               MOVE 0 TO XB-POSTED
               DISPLAY "STERP-W-3401 the conversion was rolled "
                       "back - nothing was loaded"
           ELSE
               MOVE "COMT" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-ERROR
                   MOVE 12 TO XB-RC
                   DISPLAY "STERP-E-3402 the conversion commit "
                           "failed " SQLIO-ERRMSG(1:60)
               ELSE
                   DISPLAY "STERP-I-3403 committed " XB-POSTED
                           " ledger items for "
                           FUNCTION TRIM(WS-BATCH-REF)
               END-IF
           END-IF
           MOVE "N" TO WS-TXN-OPEN.
       3400-EXIT.
           EXIT.

       3500-CLOSE-BATCH.
           IF NOT XB-COMMIT OR WS-BATCH-ID = 0
               GO TO 3500-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE stg.ImportBatch SET RowCountTotal = ?, "
                  "RowCountAccepted = ?, RowCountRejected = ?, "
                  "RowCountDuplicate = ?, Status = ?, CompletedAt "
                  "= SYSDATETIME() WHERE BatchId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-ROWS-IN TO XB-SEQ-ED
           MOVE FUNCTION TRIM(XB-SEQ-ED) TO SQLIO-PARM-VAL(1)
           MOVE XB-ACCEPTED TO XB-SEQ-ED
           MOVE FUNCTION TRIM(XB-SEQ-ED) TO SQLIO-PARM-VAL(2)
           MOVE XB-REJECTED TO XB-SEQ-ED
           MOVE FUNCTION TRIM(XB-SEQ-ED) TO SQLIO-PARM-VAL(3)
           MOVE XB-DUPLICATE TO XB-SEQ-ED
           MOVE FUNCTION TRIM(XB-SEQ-ED) TO SQLIO-PARM-VAL(4)
           IF XB-REJECTED > 0 OR XB-DUPLICATE > 0
               MOVE "PARTIAL" TO SQLIO-PARM-VAL(5)
           ELSE
               MOVE "COMMITTED" TO SQLIO-PARM-VAL(5)
           END-IF
           MOVE WS-BATCH-ID TO XB-SEQ-ED
           MOVE FUNCTION TRIM(XB-SEQ-ED) TO SQLIO-PARM-VAL(6)
           PERFORM VARYING XB-J FROM 1 BY 1 UNTIL XB-J > 6
               MOVE "N" TO SQLIO-PARM-NULL(XB-J)
           END-PERFORM
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       3500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> validation helpers - XB-K carries the yes/no answer
      *>----------------------------------------------------------------
       7000-COUNT-CHECK.
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           PERFORM VARYING XB-J FROM 1 BY 1 UNTIL XB-J > WS-PCNT
               MOVE "N" TO SQLIO-PARM-NULL(XB-J)
           END-PERFORM
           MOVE WS-PCNT TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           MOVE 0 TO XB-K
           IF SQLIO-OK
               COMPUTE XB-K = FUNCTION NUMVAL(BR-COL(1))
           END-IF.
       7000-EXIT.
           EXIT.

       7100-CHECK-DATE.
      *>   XB-TEXT(1:8) -> XB-NUM-DATE, XB-K = 1 when it is real
           MOVE 0 TO XB-K
           MOVE 0 TO XB-NUM-DATE
           IF XB-TEXT(1:8) NOT NUMERIC
               GO TO 7100-EXIT
           END-IF
           COMPUTE XB-NUM-DATE = FUNCTION NUMVAL(XB-TEXT(1:8))
           IF XB-NUM-DATE < 19000101
               MOVE 0 TO XB-NUM-DATE
               GO TO 7100-EXIT
           END-IF
           IF FUNCTION TEST-DATE-YYYYMMDD(XB-NUM-DATE) = 0
               MOVE 1 TO XB-K
           ELSE
               MOVE 0 TO XB-NUM-DATE
           END-IF.
       7100-EXIT.
           EXIT.

       7200-CHECK-NUM.
      *>   XB-TEXT -> XB-AMT, XB-K = 1 when the text is a number
           MOVE 0 TO XB-K
           MOVE 0 TO XB-AMT
           MOVE SPACES TO XB-TEXT2
           MOVE FUNCTION TRIM(XB-TEXT) TO XB-TEXT2
           IF XB-TEXT2 = SPACES
               GO TO 7200-EXIT
           END-IF
           MOVE 1 TO XB-K
           PERFORM VARYING XB-I FROM 1 BY 1 UNTIL XB-I > 20
               IF XB-TEXT2(XB-I:1) NOT = SPACE
                   AND XB-TEXT2(XB-I:1) NOT = "-"
                   AND XB-TEXT2(XB-I:1) NOT = "+"
                   AND XB-TEXT2(XB-I:1) NOT = "."
                   AND XB-TEXT2(XB-I:1) NOT = ","
                   AND XB-TEXT2(XB-I:1) NOT NUMERIC
                   MOVE 0 TO XB-K
               END-IF
           END-PERFORM
           IF XB-K = 1
               COMPUTE XB-AMT = FUNCTION NUMVAL(XB-TEXT2)
           END-IF.
       7200-EXIT.
           EXIT.

       COPY XMDBATPR.
