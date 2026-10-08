      *>================================================================
      *> PROGRAM-ID : EXCPAGE
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : CROSS MODULE CONTROLS AND REPORTING (XMD)
      *> PURPOSE    : EXCEPTION AGEING, OWNERSHIP AND ESCALATION, AND
      *>              THE AUDIT, APPROVALS AND DATA QUALITY REVIEW
      *>              LISTING.  EVERY OPEN CROSS MODULE EXCEPTION IS
      *>              AGED FROM THE MOMENT IT WAS FIRST DETECTED, NOT
      *>              FROM THE LAST TIME IT WAS PASSED ON, SO THAT
      *>              REASSIGNING AN EXCEPTION CANNOT RESET ITS AGE
      *>              OR ERASE THE PREVIOUS OWNER.  ITEMS PAST THEIR
      *>              DUE DATE OR OLDER THAN THE ESCALATION AGE ARE
      *>              ESCALATED TO THE FINANCIAL CONTROLLER WITH THE
      *>              ORIGINAL OWNER RETAINED IN THE MESSAGE.
      *> RUN AS     : EXCPAGE CO=STC ASAT=20261031 MODE=COMMIT
      *>              USER=FINCTRL [OWNER=APMGR] [DAYS=30]
      *> OUTPUT     : run\spool\EXCPAGE-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, PARMGET, EXCPRAIS,
      *>              SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-01-24 RJH  CR0271 ORIGINAL EXCEPTION DIARY LISTING
      *> 1996-09-30 RJH  CR0514 OWNER AND ESCALATION COLUMNS
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS THROUGHOUT
      *> 2026-10-06 STERP XMD-006 AGE SURVIVES REASSIGNMENT, RPT-016
      *>                 APPROVALS AND DATA QUALITY SECTIONS ADDED
      *>================================================================
      *> REQ: XMD-006, XMD-009, COM-022, COM-017, RPT-016
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXCPAGE.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 24/01/1994.
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
       FD  RPT-FILE REPORT IS R-EXCPAGE.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY XMDBATWK.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARMGET==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXCPRAIS==.
       01  WS-DET.
           05  D-OWNER                 PIC X(20) VALUE SPACES.
           05  D-ID                    PIC 9(9) VALUE 0.
           05  D-MODULE                PIC X(6) VALUE SPACES.
           05  D-SEVERITY              PIC X(8) VALUE SPACES.
           05  D-CATEGORY              PIC X(30) VALUE SPACES.
           05  D-DOCTYPE               PIC X(12) VALUE SPACES.
           05  D-DOCREF                PIC X(24) VALUE SPACES.
           05  D-RAISED                PIC X(10) VALUE SPACES.
           05  D-DUE                   PIC X(10) VALUE SPACES.
           05  D-AGE                   PIC S9(5) VALUE 0.
           05  D-AMOUNT                PIC S9(13)V99 VALUE 0.
           05  D-STATUS                PIC X(12) VALUE SPACES.
           05  D-FLAG                  PIC X(10) VALUE SPACES.
           05  D-TEXT                  PIC X(100) VALUE SPACES.
       01  WS-BUCKET.
           05  B-0007                  PIC S9(5) VALUE 0.
           05  B-0830                  PIC S9(5) VALUE 0.
           05  B-3160                  PIC S9(5) VALUE 0.
           05  B-61UP                  PIC S9(5) VALUE 0.
           05  B-V0007                 PIC S9(13)V99 VALUE 0.
           05  B-V0830                 PIC S9(13)V99 VALUE 0.
           05  B-V3160                 PIC S9(13)V99 VALUE 0.
           05  B-V61UP                 PIC S9(13)V99 VALUE 0.
           05  B-UNOWNED               PIC S9(5) VALUE 0.
           05  B-HOLDS                 PIC S9(5) VALUE 0.
       01  WS-RPT16.
           05  R-APPROVALS             PIC S9(5) VALUE 0.
           05  R-APPR-OLD              PIC S9(5) VALUE 0.
           05  R-OVERRIDES             PIC S9(5) VALUE 0.
           05  R-SENSITIVE             PIC S9(5) VALUE 0.
           05  R-DQ                    PIC S9(5) VALUE 0.
           05  R-LABEL                 PIC X(52) VALUE SPACES.
           05  R-VALUE                 PIC S9(9) VALUE 0.
           05  R-NOTE                  PIC X(60) VALUE SPACES.
       01  WS-SCOPE.
           05  S-ASAT                  PIC X(10) VALUE SPACES.
           05  S-OWNER                 PIC X(20) VALUE "ALL".
           05  S-DAYS                  PIC 9(4) VALUE 30.
           05  S-MODE                  PIC X(10) VALUE SPACES.
       01  WS-LAST-OWNER               PIC X(20) VALUE SPACES.
       REPORT SECTION.
       RD  R-EXCPAGE
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 8 LAST DETAIL 52
           FOOTING 58.
       COPY STRPTHDR.
       01  DE-SCOPE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(10) VALUE "As at     ".
               10  COLUMN 11   PIC X(10) SOURCE S-ASAT.
               10  COLUMN 26   PIC X(14) VALUE "Owner filter  ".
               10  COLUMN 40   PIC X(20) SOURCE S-OWNER.
               10  COLUMN 64   PIC X(18) VALUE "Escalation age    ".
               10  COLUMN 82   PIC ZZZ9 SOURCE S-DAYS.
               10  COLUMN 87   PIC X(5) VALUE "days ".
               10  COLUMN 96   PIC X(10) SOURCE S-MODE.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE
                   "Open and acknowledged exceptions only. Age runs".
               10  COLUMN 48   PIC X(62) VALUE
                   "from first detection, not from reassignment.".
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(5) VALUE "Owner".
               10  COLUMN 22   PIC X(2) VALUE "Id".
               10  COLUMN 30   PIC X(3) VALUE "Mod".
               10  COLUMN 35   PIC X(8) VALUE "Severity".
               10  COLUMN 45   PIC X(8) VALUE "Category".
               10  COLUMN 77   PIC X(8) VALUE "Document".
               10  COLUMN 92   PIC X(6) VALUE "Raised".
               10  COLUMN 104  PIC X(3) VALUE "Age".
               10  COLUMN 109  PIC X(11) VALUE "Amount GBP ".
               10  COLUMN 124  PIC X(6) VALUE "Flag  ".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-EXC TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(20) SOURCE D-OWNER.
               10  COLUMN 22   PIC ZZZZZZZZ9 SOURCE D-ID.
               10  COLUMN 32   PIC X(6) SOURCE D-MODULE.
               10  COLUMN 39   PIC X(8) SOURCE D-SEVERITY.
               10  COLUMN 48   PIC X(28) SOURCE D-CATEGORY.
               10  COLUMN 77   PIC X(14) SOURCE D-DOCREF.
               10  COLUMN 92   PIC X(10) SOURCE D-RAISED.
               10  COLUMN 103  PIC -ZZZ9 SOURCE D-AGE.
               10  COLUMN 109  PIC ZZZ,ZZZ,ZZ9.99- SOURCE D-AMOUNT.
               10  COLUMN 124  PIC X(9) SOURCE D-FLAG.
       01  DE-ETXT TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 24   PIC X(100) SOURCE D-TEXT.
               10  COLUMN 126  PIC X(6) SOURCE D-STATUS.
       01  DE-SUM TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(52) SOURCE R-LABEL.
               10  COLUMN 58   PIC Z(8)9 SOURCE R-VALUE.
               10  COLUMN 70   PIC X(60) SOURCE R-NOTE.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(18) VALUE "Ageing buckets    ".
               10  COLUMN 20   PIC X(9) VALUE "0-7 days ".
               10  COLUMN 29   PIC ZZZ9 SOURCE B-0007.
               10  COLUMN 35   PIC ZZZ,ZZZ,ZZ9.99- SOURCE B-V0007.
               10  COLUMN 54   PIC X(10) VALUE "8-30 days ".
               10  COLUMN 64   PIC ZZZ9 SOURCE B-0830.
               10  COLUMN 70   PIC ZZZ,ZZZ,ZZ9.99- SOURCE B-V0830.
           05  LINE PLUS 1.
               10  COLUMN 20   PIC X(11) VALUE "31-60 days ".
               10  COLUMN 31   PIC ZZZ9 SOURCE B-3160.
               10  COLUMN 37   PIC ZZZ,ZZZ,ZZ9.99- SOURCE B-V3160.
               10  COLUMN 54   PIC X(14) VALUE "61 days plus  ".
               10  COLUMN 68   PIC ZZZ9 SOURCE B-61UP.
               10  COLUMN 74   PIC ZZZ,ZZZ,ZZ9.99- SOURCE B-V61UP.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(18) VALUE "Holds outstanding ".
               10  COLUMN 20   PIC ZZZ9 SOURCE B-HOLDS.
               10  COLUMN 28   PIC X(24) VALUE
                   "Exceptions without owner".
               10  COLUMN 53   PIC ZZZ9 SOURCE B-UNOWNED.
               10  COLUMN 60   PIC X(12) VALUE "Escalated   ".
               10  COLUMN 72   PIC ZZZ9 SOURCE XB-ESCALATED.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(64) VALUE
                   "Reassigning an exception does not reset its age".
               10  COLUMN 50   PIC X(62) VALUE
                   "or erase the previous owner (XMD-006).".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           MOVE "EXCPAGE" TO RPT-REPORT-ID
           MOVE "Exception ageing, ownership and escalation"
               TO RPT-TITLE
           MOVE "Audit, approvals and data quality review"
               TO RPT-SUBTITLE
           PERFORM 9100-JOB-START THRU 9100-EXIT
           PERFORM 9200-OPEN-SPOOL THRU 9200-EXIT
           IF XB-RC > 4
               GO TO 0000-FINISH
           END-IF
           INITIATE R-EXCPAGE
           GENERATE DE-SCOPE
           PERFORM 1000-LIST-EXCEPTIONS THRU 1000-EXIT
           PERFORM 2000-RPT16-SECTIONS THRU 2000-EXIT
           TERMINATE R-EXCPAGE
           PERFORM 9350-CLOSE-SPOOL THRU 9350-EXIT.
       0000-FINISH.
           PERFORM 9150-JOB-END THRU 9150-EXIT
           MOVE XB-RC TO RETURN-CODE
           STOP RUN.

       0100-READ-ARGS.
           PERFORM 9500-STD-ARGS THRU 9500-EXIT
           MOVE "XMD.EXCEPTION.ESCALATE.DAYS" TO XB-CODE
           MOVE 30 TO XB-AMT
           PERFORM 9650-GET-PARM-NUM THRU 9650-EXIT
           MOVE XB-AMT TO S-DAYS
           MOVE "DAYS" TO XB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           IF XB-TEXT NOT = SPACES
               COMPUTE S-DAYS = FUNCTION NUMVAL(XB-TEXT)
           END-IF
           MOVE "OWNER" TO XB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           IF XB-TEXT NOT = SPACES
               MOVE XB-TEXT(1:20) TO S-OWNER
               MOVE XB-TEXT(1:20) TO XB-OWNER
           END-IF
           MOVE XB-ASAT TO XB-NUM-DATE
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO S-ASAT
           MOVE XB-ASAT TO XB-DATE-ED
           MOVE XB-MODE TO S-MODE
           DISPLAY "STERP-I-0100 EXCPAGE ageing for "
                   FUNCTION TRIM(XB-CO) " as at " XB-ASAT.
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: XMD-006, COM-022  one accountable owner, the contributing
      *> record, the next action and the age since first detection
      *>----------------------------------------------------------------
       1000-LIST-EXCEPTIONS.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),e.ExceptionId), "
                  "ISNULL(NULLIF(e.OwnerUser,''),ISNULL(NULLIF("
                  "e.OwnerRole,''),'(UNOWNED)')), e.Module, "
                  "e.Severity, e.Category, ISNULL(e.DocumentType,''"
                  "), ISNULL(e.DocumentRef,''), CONVERT(char(8),"
                  "e.RaisedAt,112), ISNULL(CONVERT(char(8),e.DueAt,"
                  "112),''), CONVERT(varchar(24),ISNULL(e.AmountGBP"
                  ",0)), e.Status, LEFT(ISNULL(e.Message,''),100), "
                  "CONVERT(varchar(12),DATEDIFF(day,e.RaisedAt,"
                  "CONVERT(date,?,112))) FROM core.ExceptionItem e "
                  "WHERE e.CompanyCode = ? AND e.Status IN ('OPEN',"
                  "'ACKNOWLEDGED') AND CONVERT(char(8),e.RaisedAt,"
                  "112) <= ? AND (? = 'ALL' OR e.OwnerUser = ? OR "
                  "e.OwnerRole = ?) ORDER BY ISNULL(NULLIF("
                  "e.OwnerUser,''),ISNULL(NULLIF(e.OwnerRole,''),"
                  "'(UNOWNED)')), e.RaisedAt"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(1) SQLIO-PARM-VAL(3)
           MOVE XB-CO TO SQLIO-PARM-VAL(2)
           MOVE S-OWNER TO SQLIO-PARM-VAL(4) SQLIO-PARM-VAL(5)
                           SQLIO-PARM-VAL(6)
           PERFORM VARYING XB-J FROM 1 BY 1 UNTIL XB-J > 6
               MOVE "N" TO SQLIO-PARM-NULL(XB-J)
           END-PERFORM
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE 13 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO XB-RC
               DISPLAY "STERP-E-1001 " SQLIO-ERRMSG(1:100)
               GO TO 1000-EXIT
           END-IF
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-OK
                   PERFORM 1100-ONE-EXCEPTION THRU 1100-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           DISPLAY "STERP-I-1002 " XB-ROWS-OUT
                   " open exception(s), " XB-ESCALATED
                   " escalated".
       1000-EXIT.
           EXIT.

       1100-ONE-EXCEPTION.
           COMPUTE D-ID = FUNCTION NUMVAL(BR-COL(1))
           MOVE BR-COL(2) TO D-OWNER
           MOVE BR-COL(3) TO D-MODULE
           MOVE BR-COL(4) TO D-SEVERITY
           MOVE BR-COL(5) TO D-CATEGORY
           MOVE BR-COL(6) TO D-DOCTYPE
           MOVE BR-COL(7) TO D-DOCREF
           MOVE BR-COL(8) TO XB-TEXT
           PERFORM 9470-ISO-TO-NUM THRU 9470-EXIT
           MOVE XB-NUM-DATE TO XB-NUM-DATE
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO D-RAISED
           MOVE BR-COL(9) TO XB-TEXT
           PERFORM 9470-ISO-TO-NUM THRU 9470-EXIT
           MOVE XB-NUM-DATE TO XB-DATE-ED2
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO D-DUE
           COMPUTE D-AMOUNT = FUNCTION NUMVAL(BR-COL(10))
           MOVE BR-COL(11) TO D-STATUS
           MOVE BR-COL(12) TO D-TEXT
           COMPUTE D-AGE = FUNCTION NUMVAL(BR-COL(13))
           MOVE SPACES TO D-FLAG
           IF D-OWNER = "(UNOWNED)"
               ADD 1 TO B-UNOWNED
               MOVE "NO OWNER" TO D-FLAG
           END-IF
           IF D-SEVERITY = "HOLD"
               ADD 1 TO B-HOLDS
           END-IF
           EVALUATE TRUE
               WHEN D-AGE <= 7
                   ADD 1 TO B-0007
                   ADD D-AMOUNT TO B-V0007
               WHEN D-AGE <= 30
                   ADD 1 TO B-0830
                   ADD D-AMOUNT TO B-V0830
               WHEN D-AGE <= 60
                   ADD 1 TO B-3160
                   ADD D-AMOUNT TO B-V3160
               WHEN OTHER
                   ADD 1 TO B-61UP
                   ADD D-AMOUNT TO B-V61UP
           END-EVALUATE
           IF XB-DATE-ED2 > 0 AND XB-DATE-ED2 < XB-ASAT
               MOVE "PAST DUE" TO D-FLAG
           END-IF
           IF D-AGE > S-DAYS
               MOVE "ESCALATE" TO D-FLAG
           END-IF
           ADD 1 TO XB-ROWS-OUT
           GENERATE DE-EXC
           IF D-TEXT NOT = SPACES
               GENERATE DE-ETXT
           END-IF
           IF D-FLAG = "ESCALATE" OR D-FLAG = "NO OWNER"
               PERFORM 1200-ESCALATE THRU 1200-EXIT
           END-IF.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: XMD-006  escalation names the original owner, so the
      *> history of the exception is not lost when it moves on
      *>----------------------------------------------------------------
       1200-ESCALATE.
           INITIALIZE WS-EXCPRAIS
           MOVE "WARNING" TO EX-SEVERITY
           MOVE "EXCEPTION ESCALATION" TO EX-CATEGORY
           MOVE "EXCEPTION" TO EX-DOCUMENT-TYPE
           MOVE SPACES TO XB-TEXT
           MOVE D-ID TO XB-SEQ-ED
           STRING "EXC" FUNCTION TRIM(XB-SEQ-ED)
               DELIMITED BY SIZE INTO XB-TEXT
           MOVE XB-TEXT(1:40) TO EX-DOCUMENT-REF
           MOVE D-AGE TO XB-INT-ED
           MOVE SPACES TO XB-TEXT2
           STRING FUNCTION TRIM(D-CATEGORY)
                  " on " FUNCTION TRIM(D-DOCREF)
                  " has been open " FUNCTION TRIM(XB-INT-ED)
                  " days with owner " FUNCTION TRIM(D-OWNER)
                  ". Escalated to the financial controller; the "
                  "age and the previous owner are retained."
               DELIMITED BY SIZE INTO XB-TEXT2
           MOVE XB-TEXT2(1:200) TO EX-MESSAGE-TEXT
           MOVE "FINCTRL" TO EX-OWNER-ROLE
           MOVE 2 TO EX-PRIORITY
           MOVE D-AMOUNT TO EX-AMOUNT-GBP
           MOVE XB-ASAT TO EX-DUE-AT
           PERFORM 9700-RAISE-EXCEPTION THRU 9700-EXIT
           ADD 1 TO XB-ESCALATED
           PERFORM 1300-STAMP-ESCALATION THRU 1300-EXIT.
       1200-EXIT.
           EXIT.

       1300-STAMP-ESCALATION.
           IF NOT XB-COMMIT
               GO TO 1300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE core.ExceptionItem SET EscalatedAt = "
                  "SYSDATETIME(), AgeDays = ? WHERE ExceptionId = ?"
                  " AND EscalatedAt IS NULL"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE D-AGE TO XB-INT-ED
           MOVE FUNCTION TRIM(XB-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE D-ID TO XB-SEQ-ED
           MOVE FUNCTION TRIM(XB-SEQ-ED) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       1300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: RPT-016, XMD-009  approvals, overrides and data quality
      *> for the authorised reviewer.  counts only - the sensitive
      *> field values stay inside their own module reports
      *>----------------------------------------------------------------
       2000-RPT16-SECTIONS.
           MOVE SPACES TO R-LABEL
           MOVE 0 TO R-VALUE
           MOVE "Audit, approvals and data quality (RPT-016)"
               TO R-LABEL
           MOVE SPACES TO R-NOTE
           GENERATE DE-SUM
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)), CONVERT("
                  "varchar(12),SUM(CASE WHEN DATEDIFF(day,"
                  "RequestedAt,CONVERT(date,?,112)) > 5 THEN 1 ELSE"
                  " 0 END)) FROM core.Approval WHERE CompanyCode = "
                  "? AND Status = 'PENDING'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(1)
           MOVE XB-CO TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-OK
               COMPUTE R-APPROVALS = FUNCTION NUMVAL(BR-COL(1))
               COMPUTE R-APPR-OLD = FUNCTION NUMVAL(BR-COL(2))
           END-IF
           MOVE "Approvals pending" TO R-LABEL
           MOVE R-APPROVALS TO R-VALUE
           MOVE SPACES TO R-NOTE
           MOVE R-APPR-OLD TO XB-INT-ED
           STRING "of which older than five days: "
                  FUNCTION TRIM(XB-INT-ED)
               DELIMITED BY SIZE INTO R-NOTE
           GENERATE DE-SUM
           PERFORM 2100-AUDIT-COUNTS THRU 2100-EXIT
           PERFORM 2200-DATA-QUALITY THRU 2200-EXIT.
       2000-EXIT.
           EXIT.

       2100-AUDIT-COUNTS.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),SUM(CASE WHEN "
                  "ISNULL(AuthorityRef,'') <> '' THEN 1 ELSE 0 END)"
                  "), CONVERT(varchar(12),SUM(CASE WHEN "
                  "ISNULL(IsSensitive,0) = 1 THEN 1 ELSE 0 END)) "
                  "FROM core.AuditLog WHERE CompanyCode = ? AND "
                  "CONVERT(char(8),EventTime,112) <= ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-OK
               COMPUTE R-OVERRIDES = FUNCTION NUMVAL(BR-COL(1))
               COMPUTE R-SENSITIVE = FUNCTION NUMVAL(BR-COL(2))
           END-IF
           MOVE "Audited actions carrying an authority reference"
               TO R-LABEL
           MOVE R-OVERRIDES TO R-VALUE
           MOVE "overrides and approvals evidenced in core.AuditLog"
               TO R-NOTE
           GENERATE DE-SUM
           MOVE "Restricted audit rows (authorised reviewers only)"
               TO R-LABEL
           MOVE R-SENSITIVE TO R-VALUE
           MOVE "values withheld from this listing and its export"
               TO R-NOTE
           GENERATE DE-SUM.
       2100-EXIT.
           EXIT.

       2200-DATA-QUALITY.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(Category,'(none)'), CONVERT("
                  "varchar(12),COUNT(*)) FROM core.ExceptionItem "
                  "WHERE CompanyCode = ? AND Status <> 'RESOLVED' "
                  "AND CONVERT(char(8),RaisedAt,112) <= ? GROUP BY "
                  "Category ORDER BY COUNT(*) DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               GO TO 2200-EXIT
           END-IF
           MOVE 0 TO XB-J
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR OR XB-J > 15
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-OK
                   ADD 1 TO XB-J
                   ADD 1 TO R-DQ
                   MOVE SPACES TO R-LABEL
                   STRING "  by category: " BR-COL(1)(1:36)
                       DELIMITED BY SIZE INTO R-LABEL
                   COMPUTE R-VALUE = FUNCTION NUMVAL(BR-COL(2))
                   MOVE SPACES TO R-NOTE
                   GENERATE DE-SUM
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       2200-EXIT.
           EXIT.

       COPY XMDBATPR.
