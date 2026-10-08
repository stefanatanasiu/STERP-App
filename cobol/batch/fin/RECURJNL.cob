      *>================================================================
      *> PROGRAM-ID : RECURJNL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER (FIN)
      *> PURPOSE    : RECURRING JOURNAL AND ACCRUAL/PREPAYMENT RELEASE
      *>              RUN.  EVERY ACTIVE SCHEDULE WHOSE NEXT DUE DATE
      *>              FALLS ON OR BEFORE THE RUN DATE IS RELEASED
      *>              THROUGH THE ONLINE ACCRREL SERVICE, WHICH OWNS
      *>              THE POSTING AND THE OCCURRENCE BOOK-KEEPING.
      *>              AC-011: RUNNING THE SAME PERIOD TWICE MUST NOT
      *>              DOUBLE POST.  AN OCCURRENCE ALREADY POSTED IS
      *>              REPORTED AS "ALREADY POSTED" AND SKIPPED, SO A
      *>              SCHEDULE OF THREE 100.00 OCCURRENCES TOTALS
      *>              300.00 AND NEVER 400.00.  A RELEASE THAT WOULD
      *>              EXCEED THE SOURCE AMOUNT, OR THAT FALLS IN A
      *>              CLOSED PERIOD, IS BLOCKED WITH A REASON - NEVER
      *>              SILENTLY POSTPONED OR REDATED.
      *> RUN AS     : RECURJNL CO=STC ASAT=20261130 MODE=COMMIT
      *>              [CODE=PREPAY01] USER=GLCLERK
      *> OUTPUT     : run\spool\RECURJNL-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO, ACCRREL
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-08-22 PGW  CR0291 ORIGINAL STANDING JOURNAL RUN
      *> 1996-02-11 RJH  CR0412 DUPLICATE RUN PROTECTION AFTER THE
      *>                 FEBRUARY DOUBLE POSTING INCIDENT
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS
      *> 2026-10-06 STERP FIN-007/FIN-008 RELEASE THROUGH ACCRREL
      *>================================================================
      *> REQ: FIN-007, FIN-008
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECURJNL.
       AUTHOR. P G WHITELAW.
       DATE-WRITTEN. 22/08/1994.
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
       FD  RPT-FILE REPORT IS R-RECURJNL.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY FINBATWK.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY ACCRRELCA REPLACING ==DFHCOMMAREA== BY ==WS-ACCRREL-CA==
                      LEADING ==CA-== BY ==AH-==.
      *>   OCCURS 200 is the 1994 limit - a company with more live
      *>   schedules than this must be run by code (CODE=)
       01  WS-LIST.
           05  WS-LIST-CNT             PIC S9(4) VALUE 0.
           05  WS-ITEM OCCURS 200 TIMES.
               10  WS-I-CODE           PIC X(20).
               10  WS-I-NAME           PIC X(80).
               10  WS-I-KIND           PIC X(10).
               10  WS-I-DUE            PIC 9(8).
               10  WS-I-SOURCE         PIC S9(11)V99.
               10  WS-I-REMAIN         PIC S9(11)V99.
       01  WS-DET.
           05  D-CODE                  PIC X(20) VALUE SPACES.
           05  D-NAME                  PIC X(34) VALUE SPACES.
           05  D-KIND                  PIC X(10) VALUE SPACES.
           05  D-OCC                   PIC 9(4) VALUE 0.
           05  D-DUE                   PIC X(10) VALUE SPACES.
           05  D-AMOUNT                PIC S9(11)V99 VALUE 0.
           05  D-REMAIN                PIC S9(11)V99 VALUE 0.
           05  D-JNL                   PIC X(20) VALUE SPACES.
           05  D-OUTCOME               PIC X(16) VALUE SPACES.
           05  D-REASON                PIC X(50) VALUE SPACES.
       01  WS-TOT.
           05  T-RELEASED              PIC S9(13)V99 VALUE 0.
           05  T-SKIPPED               PIC S9(5) VALUE 0.
           05  T-BLOCKED               PIC S9(5) VALUE 0.
           05  T-POSTED                PIC S9(5) VALUE 0.
       REPORT SECTION.
       RD  R-RECURJNL
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 52
           FOOTING 58.
       COPY STRPTHDR.
       01  PH-COLS TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(4) VALUE "Code".
               10  COLUMN 22   PIC X(4) VALUE "Name".
               10  COLUMN 57   PIC X(4) VALUE "Kind".
               10  COLUMN 68   PIC X(3) VALUE "Occ".
               10  COLUMN 74   PIC X(8) VALUE "Due date".
               10  COLUMN 88   PIC X(6) VALUE "Amount".
               10  COLUMN 101  PIC X(9) VALUE "Remaining".
               10  COLUMN 114  PIC X(7) VALUE "Outcome".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(20) SOURCE D-CODE.
               10  COLUMN 22   PIC X(34) SOURCE D-NAME.
               10  COLUMN 57   PIC X(10) SOURCE D-KIND.
               10  COLUMN 68   PIC ZZZ9 SOURCE D-OCC.
               10  COLUMN 74   PIC X(10) SOURCE D-DUE.
               10  COLUMN 85   PIC ZZZ,ZZ9.99- SOURCE D-AMOUNT.
               10  COLUMN 98   PIC ZZZ,ZZ9.99- SOURCE D-REMAIN.
               10  COLUMN 112  PIC X(16) SOURCE D-OUTCOME.
       01  DE-WHY TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 22   PIC X(10) VALUE "          ".
               10  COLUMN 24   PIC X(20) SOURCE D-JNL.
               10  COLUMN 46   PIC X(50) SOURCE D-REASON.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(20) VALUE "Released this run   ".
               10  COLUMN 22   PIC ZZZ,ZZ9 SOURCE T-POSTED.
               10  COLUMN 34   PIC X(14) VALUE "occurrence(s) ".
               10  COLUMN 50   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-RELEASED.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(20) VALUE "Already posted      ".
               10  COLUMN 22   PIC ZZZ,ZZ9 SOURCE T-SKIPPED.
               10  COLUMN 34   PIC X(40) VALUE
                   "(a repeated run never posts twice)".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(20) VALUE "Blocked             ".
               10  COLUMN 22   PIC ZZZ,ZZ9 SOURCE T-BLOCKED.
               10  COLUMN 34   PIC X(50) VALUE
                   "(closed period or release beyond the source)".
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(60) SOURCE RPT-STATUS-TEXT.
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           MOVE "RECURJNL" TO RPT-REPORT-ID
           PERFORM 9100-JOB-START THRU 9100-EXIT
           PERFORM 9200-OPEN-SPOOL THRU 9200-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           INITIATE R-RECURJNL
           PERFORM 1000-SELECT-DUE THRU 1000-EXIT
           PERFORM 2000-RELEASE-ALL THRU 2000-EXIT
           TERMINATE R-RECURJNL
           PERFORM 9350-CLOSE-SPOOL THRU 9350-EXIT.
       0000-FINISH.
           PERFORM 9150-JOB-END THRU 9150-EXIT
           MOVE WS-RC TO RETURN-CODE
           STOP RUN.

       0100-READ-ARGS.
           PERFORM 9500-STD-ARGS THRU 9500-EXIT
           MOVE "CODE" TO FB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           MOVE FB-TEXT(1:20) TO WS-CODE
           IF WS-MODE = SPACES
               MOVE "PREVIEW" TO WS-MODE
           END-IF
           IF WS-MODE NOT = "COMMIT" AND WS-MODE NOT = "PREVIEW"
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0101 MODE must be PREVIEW or COMMIT"
               GO TO 0100-EXIT
           END-IF
           DISPLAY "STERP-I-0100 RECURJNL " FUNCTION TRIM(WS-MODE)
                   " run for " FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-007  select the schedules that are due; the list is
      *> taken once so that a schedule rolled forward by this run is
      *> not picked up a second time in the same pass
      *>----------------------------------------------------------------
       1000-SELECT-DUE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT RecurringCode, Name, JournalKind, CONVERT"
                  "(char(8),NextDueDate,112), CONVERT(varchar(24),"
                  "SourceAmount), CONVERT(varchar(24),"
                  "RemainingAmount) FROM fin.RecurringJournal WHERE "
                  "CompanyCode = ? AND Status = 'ACTIVE' AND "
                  "NextDueDate IS NOT NULL AND CONVERT(char(8),"
                  "NextDueDate,112) <= ? AND (? = '' OR "
                  "RecurringCode = ?) ORDER BY NextDueDate, "
                  "RecurringCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-ASAT TO FB-DATE-ED
           MOVE FB-DATE-ED TO SQLIO-PARM-VAL(2)
           MOVE WS-CODE TO SQLIO-PARM-VAL(3) SQLIO-PARM-VAL(4)
           PERFORM VARYING FB-I FROM 1 BY 1 UNTIL FB-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(FB-I)
           END-PERFORM
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE 6 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1001 " SQLIO-ERRMSG
               GO TO 1000-EXIT
           END-IF
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-OK AND WS-LIST-CNT < 200
                   ADD 1 TO WS-LIST-CNT
                   MOVE BR-COL(1) TO WS-I-CODE(WS-LIST-CNT)
                   MOVE BR-COL(2) TO WS-I-NAME(WS-LIST-CNT)
                   MOVE BR-COL(3) TO WS-I-KIND(WS-LIST-CNT)
                   COMPUTE WS-I-DUE(WS-LIST-CNT) =
                       FUNCTION NUMVAL(BR-COL(4))
                   COMPUTE WS-I-SOURCE(WS-LIST-CNT) =
                       FUNCTION NUMVAL(BR-COL(5))
                   COMPUTE WS-I-REMAIN(WS-LIST-CNT) =
                       FUNCTION NUMVAL(BR-COL(6))
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           DISPLAY "STERP-I-1002 " WS-LIST-CNT " schedule(s) due".
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       2000-RELEASE-ALL.
           MOVE "Recurring journal and accrual release" TO RPT-TITLE
           MOVE WS-ASAT TO FB-NUM-DATE
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE SPACES TO RPT-SUBTITLE
           STRING "Schedules due on or before " WS-UK
               DELIMITED BY SIZE INTO RPT-SUBTITLE
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "Mode " FUNCTION TRIM(WS-MODE)
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           GENERATE PH-COLS
           PERFORM VARYING FB-K FROM 1 BY 1
                   UNTIL FB-K > WS-LIST-CNT
               PERFORM 2100-ONE-SCHEDULE THRU 2100-EXIT
           END-PERFORM
           MOVE SPACES TO RPT-STATUS-TEXT
           IF WS-MODE = "PREVIEW"
               MOVE "PREVIEW ONLY - nothing has been posted"
                   TO RPT-STATUS-TEXT
           ELSE
               MOVE "Postings are complete and committed"
                   TO RPT-STATUS-TEXT
           END-IF
           MOVE T-POSTED TO WS-POSTED
           MOVE T-BLOCKED TO WS-BLOCKED
           MOVE WS-LIST-CNT TO WS-ROWS-OUT
           IF T-BLOCKED > 0
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-2001 " T-BLOCKED
                       " schedule(s) blocked - see the exception log"
           END-IF
           DISPLAY "STERP-I-2002 " T-POSTED " released, "
                   T-SKIPPED " already posted".
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-008  ACCRREL owns the posting; this driver only
      *> decides which schedules to offer it and how to report back
      *>----------------------------------------------------------------
       2100-ONE-SCHEDULE.
           MOVE WS-I-CODE(FB-K) TO D-CODE
           MOVE WS-I-NAME(FB-K) TO D-NAME
           MOVE WS-I-KIND(FB-K) TO D-KIND
           MOVE WS-I-DUE(FB-K) TO FB-NUM-DATE
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE WS-UK TO D-DUE
           MOVE SPACES TO D-JNL
           MOVE SPACES TO D-REASON
           PERFORM 2200-NEXT-OCCURRENCE THRU 2200-EXIT
           IF AH-RETURN-CODE > 4
               MOVE "NOT DUE" TO D-OUTCOME
               MOVE AH-MESSAGE TO D-REASON
               MOVE 0 TO D-AMOUNT
               MOVE WS-I-REMAIN(FB-K) TO D-REMAIN
               GENERATE DE-LINE
               GENERATE DE-WHY
               GO TO 2100-EXIT
           END-IF
           MOVE AK-OCCURRENCE-NO TO D-OCC
           MOVE AK-RELEASE-AMOUNT TO D-AMOUNT
           MOVE AK-REMAINING TO D-REMAIN
           IF AK-ALREADY-POSTED = "Y"
      *>       AC-011 the second run of the same occurrence is a
      *>       no-op; the journal number of the first run is shown
               MOVE "ALREADY POSTED" TO D-OUTCOME
               MOVE AK-JOURNAL-NUMBER TO D-JNL
               MOVE "repeat run - no second posting made"
                   TO D-REASON
               ADD 1 TO T-SKIPPED
               GENERATE DE-LINE
               GENERATE DE-WHY
               GO TO 2100-EXIT
           END-IF
           IF AK-BLOCKED = "Y"
               MOVE "BLOCKED" TO D-OUTCOME
               MOVE AK-BLOCK-REASON TO D-REASON
               ADD 1 TO T-BLOCKED
               GENERATE DE-LINE
               GENERATE DE-WHY
               GO TO 2100-EXIT
           END-IF
           IF WS-MODE = "PREVIEW"
               MOVE "DUE (PREVIEW)" TO D-OUTCOME
               GENERATE DE-LINE
               GO TO 2100-EXIT
           END-IF
           PERFORM 2300-RELEASE THRU 2300-EXIT
           GENERATE DE-LINE
           IF D-REASON NOT = SPACES
               GENERATE DE-WHY
           END-IF.
       2100-EXIT.
           EXIT.

       2200-NEXT-OCCURRENCE.
           INITIALIZE WS-ACCRREL-CA
           MOVE "NEXTOCC" TO AH-FUNCTION
           MOVE WS-CO TO AH-COMPANY
           MOVE WS-USER TO AH-USER
           MOVE WS-ASAT TO AH-BUSINESS-DATE
           MOVE WS-I-CODE(FB-K) TO AK-RECUR-CODE
           MOVE WS-ASAT TO AK-POSTING-DATE
           CALL "ACCRREL" USING WS-ACCRREL-CA.
       2200-EXIT.
           EXIT.

       2300-RELEASE.
           MOVE SPACES TO FB-CORREL
           STRING "RECURJNL-" FUNCTION TRIM(WS-I-CODE(FB-K)) "-"
               DELIMITED BY SIZE INTO FB-CORREL
           MOVE AK-OCCURRENCE-NO TO FB-SEQ-ED
           PERFORM VARYING FB-J FROM 36 BY -1
               UNTIL FB-J < 2 OR FB-CORREL(FB-J:1) NOT = SPACE
               CONTINUE
           END-PERFORM
           ADD 1 TO FB-J
           MOVE FB-SEQ-ED TO FB-CORREL(FB-J:6)
           INITIALIZE WS-ACCRREL-CA
           MOVE "RELEASE" TO AH-FUNCTION
           MOVE WS-CO TO AH-COMPANY
           MOVE WS-USER TO AH-USER
           MOVE WS-ASAT TO AH-BUSINESS-DATE
           MOVE FB-CORREL TO AH-CORRELATION-ID
           MOVE WS-I-CODE(FB-K) TO AK-RECUR-CODE
           MOVE D-OCC TO AK-OCCURRENCE-NO
           MOVE WS-ASAT TO AK-POSTING-DATE
           MOVE "COMMIT" TO AK-MODE
           CALL "ACCRREL" USING WS-ACCRREL-CA
           EVALUATE TRUE
               WHEN AH-RETURN-CODE = 0 OR AH-RETURN-CODE = 4
                   IF AK-ALREADY-POSTED = "Y"
                       MOVE "ALREADY POSTED" TO D-OUTCOME
                       ADD 1 TO T-SKIPPED
                   ELSE
                       MOVE "RELEASED" TO D-OUTCOME
                       ADD 1 TO T-POSTED
                       ADD AK-RELEASE-AMOUNT TO T-RELEASED
                   END-IF
                   MOVE AK-JOURNAL-NUMBER TO D-JNL
                   MOVE AK-REMAINING TO D-REMAIN
               WHEN OTHER
                   MOVE "BLOCKED" TO D-OUTCOME
                   MOVE AH-MESSAGE TO D-REASON
                   ADD 1 TO T-BLOCKED
           END-EVALUATE.
       2300-EXIT.
           EXIT.

       COPY FINBATPR.
