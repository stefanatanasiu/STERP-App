      *>================================================================
      *> PROGRAM-ID : STERPOPS
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : CROSS MODULE OPERATIONS (XMD)
      *> PURPOSE    : GREEN SCREEN OPERATOR CONSOLE.  THE NIGHT
      *>              OPERATOR SIGNS ON, SUBMITS BATCH WORK FROM THE
      *>              REPORT / JOB CATALOGUE, WATCHES THE JOB MONITOR,
      *>              WORKS THE EXCEPTION QUEUE, CHECKS THE LEDGER
      *>              PERIOD STATUS AND PINGS THE ENGINE.
      *>              THIS IS THE ONLY STERP PROGRAM THAT TALKS TO A
      *>              TERMINAL - EVERYTHING ELSE IS BATCH OR THE
      *>              WINDOWS CLIENT.
      *> RUN AS     : STERPOPS              interactive console
      *>              STERPOPS SMOKE        non interactive self test:
      *>                                    every screen's data is
      *>                                    printed to SYSOUT, RC=0
      *>              STERPOPS SMOKE USER=OPS CO=STC
      *> CALLS      : SQLIO, PING, SYSTEM (to launch batch work)
      *> TABLES     : core.AppUser, core.ReportDef, core.JobRun,
      *>              core.ExceptionItem, fin.Period
      *> KEYS       : F3 = exit / back, F7 = page back, F8 = page
      *>              forward, ENTER = refresh.  THE SAME COMMANDS CAN
      *>              BE TYPED (X, -, +) FOR TERMINALS WHOSE FUNCTION
      *>              KEYS ARE NOT PASSED BY THE RUN TIME.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2008-05-19 DJM  CR1877 ORIGINAL - REPLACED THE VT220 MENU
      *> 2011-03-02 DJM  CR2015 EXCEPTION QUEUE ADDED (COM-017)
      *> 2016-07-14 RJH  CR2188 JOB MONITOR PAGING, SPOOL PATH SHOWN
      *> 2026-10-06 STERP XMD-005/XMD-007 JOB STREAM LAUNCHER, SMOKE
      *>================================================================
      *> REQ: XMD-005, XMD-007, COM-018, COM-002, COM-017
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STERPOPS.
       AUTHOR. D J MERRICK.
       DATE-WRITTEN. 19/05/2008.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES.
           CRT STATUS IS WS-CRT-STATUS
           CURSOR IS WS-CURSOR-POS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY PINGCA REPLACING ==DFHCOMMAREA== BY ==WS-PING-CA==
                   LEADING ==CA-== BY ==PC-==.
      *>---------------------------------------------------------------
      *> terminal control.  1001-1064 are F1-F64, 2005 is ESC in the
      *> GnuCOBOL screen handler; anything else is treated as ENTER.
      *>---------------------------------------------------------------
       01  WS-CRT-STATUS               PIC 9(4) VALUE 0.
           88  K-ENTER                 VALUE 0.
           88  K-F3                    VALUE 1003.
           88  K-F7                    VALUE 1007.
           88  K-F8                    VALUE 1008.
           88  K-ESCAPE                VALUE 2005.
           88  K-PAGE-UP               VALUE 2001.
           88  K-PAGE-DOWN             VALUE 2002.
       01  WS-CURSOR-POS               PIC 9(6) VALUE 0.
       01  WS-FLAGS.
           05  WS-SMOKE                PIC X VALUE "N".
               88  SMOKE-RUN           VALUE "Y".
           05  WS-QUIT                 PIC X VALUE "N".
           05  WS-LEAVE                PIC X VALUE "N".
           05  WS-RC                   PIC 9(2) VALUE 0.
           05  WS-DB-OK                PIC X VALUE "N".
           05  WS-ATTEMPT              PIC 9 VALUE 0.
       01  WS-SESSION.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-USER-NAME            PIC X(40) VALUE SPACES.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-SITE                 PIC X(10) VALUE SPACES.
           05  WS-RDONLY               PIC X VALUE "N".
       01  WS-INPUT.
           05  WS-IN-USER              PIC X(20) VALUE SPACES.
           05  WS-IN-CO                PIC X(10) VALUE SPACES.
           05  WS-OPT                  PIC X(8) VALUE SPACES.
       01  WS-CMD-LINE                 PIC X(160) VALUE SPACES.
       01  WS-SYS-CMD                  PIC X(400) VALUE SPACES.
       01  WS-ARG.
           05  WS-ARG-KEY              PIC X(12) VALUE SPACES.
           05  WS-ARG-VAL              PIC X(30) VALUE SPACES.
       01  WS-WORK.
           05  WS-I                    PIC S9(4) COMP VALUE 0.
           05  WS-J                    PIC S9(4) COMP VALUE 0.
           05  WS-K                    PIC S9(4) COMP VALUE 0.
           05  WS-LEN                  PIC S9(4) COMP VALUE 0.
           05  WS-POS                  PIC S9(4) COMP VALUE 0.
           05  WS-NUM                  PIC S9(9) VALUE 0.
           05  WS-NUM-ED               PIC Z(8)9.
           05  WS-SEQ-ED               PIC ZZ9.
           05  WS-RC-ED                PIC -(3)9.
           05  WS-TEXT                 PIC X(200) VALUE SPACES.
           05  WS-RULE                 PIC X(78) VALUE ALL "-".
       01  WS-STAMP.
           05  WS-NOW                  PIC 9(16) VALUE 0.
           05  WS-NOW-R REDEFINES WS-NOW.
               10  WS-NOW-YYYY         PIC 9(4).
               10  WS-NOW-MM           PIC 9(2).
               10  WS-NOW-DD           PIC 9(2).
               10  WS-NOW-HH           PIC 9(2).
               10  WS-NOW-MI           PIC 9(2).
               10  WS-NOW-SS           PIC 9(2).
               10  FILLER              PIC 9(2).
           05  WS-TODAY                PIC 9(8) VALUE 0.
           05  WS-STAMP-14             PIC X(14) VALUE SPACES.
      *>---------------------------------------------------------------
      *> one generic list table serves every enquiry screen: the key is
      *> what the operator selects, AUX carries the hidden detail
      *> (parameter spec, spool path), TXT is the 78 column line.
      *>---------------------------------------------------------------
       01  WS-TAB.
           05  WS-ROW-CNT              PIC S9(4) COMP VALUE 0.
           05  WS-TOP                  PIC S9(4) COMP VALUE 1.
           05  WS-SEL                  PIC S9(4) COMP VALUE 0.
           05  WS-PAGE-SIZE            PIC S9(4) COMP VALUE 15.
           05  WS-ROW OCCURS 300 TIMES.
               10  WS-ROW-KEY          PIC X(20).
               10  WS-ROW-AUX          PIC X(110).
               10  WS-ROW-TXT          PIC X(78).
       01  WS-DET.
           05  WS-DET-LINE OCCURS 15 TIMES PIC X(78).
       01  WS-SCR-FIELDS.
           05  WS-HDR-TITLE            PIC X(44) VALUE SPACES.
           05  WS-HDR-DATE             PIC X(19) VALUE SPACES.
           05  WS-PAGE-TXT             PIC X(24) VALUE SPACES.
           05  WS-MSG                  PIC X(78) VALUE SPACES.
           05  WS-LEGEND               PIC X(66) VALUE SPACES.
       01  WS-PARM.
           05  WS-PARM-CNT             PIC 9(2) VALUE 0.
           05  WS-PARM-SPEC            PIC X(110) VALUE SPACES.
           05  WS-PARM-ENT OCCURS 8 TIMES.
               10  WS-PK               PIC X(14).
               10  WS-PV               PIC X(24).
       01  WS-JOB.
           05  WS-JOB-CODE             PIC X(20) VALUE SPACES.
           05  WS-JOB-PROG             PIC X(8) VALUE SPACES.
           05  WS-JOB-STREAM           PIC X VALUE "N".
           05  WS-JOB-RC               PIC S9(9) COMP-5 VALUE 0.
           05  WS-JOB-SPOOL            PIC X(110) VALUE SPACES.
           05  WS-JOB-STAT             PIC X(12) VALUE SPACES.
       01  WS-HOME                     PIC X(120) VALUE SPACES.
      *>---------------------------------------------------------------
      *> SQLIO row areas - every column arrives as left justified text
      *>---------------------------------------------------------------
       01  R-USER.
           05  RU-NAME                 PIC X(40).
           05  RU-CO                   PIC X(10).
           05  RU-SITE                 PIC X(10).
           05  RU-RDONLY               PIC X(2).
       01  R-JOB.
           05  RJ-CODE                 PIC X(20).
           05  RJ-MOD                  PIC X(6).
           05  RJ-PROG                 PIC X(8).
           05  RJ-PARMS                PIC X(100).
           05  RJ-TITLE                PIC X(40).
       01  R-RUN.
           05  RR-ID                   PIC X(12).
           05  RR-NAME                 PIC X(40).
           05  RR-CO                   PIC X(10).
           05  RR-START                PIC X(20).
           05  RR-STAT                 PIC X(12).
           05  RR-RC                   PIC X(8).
           05  RR-SPOOL                PIC X(110).
       01  R-EXC.
           05  RE-ID                   PIC X(12).
           05  RE-CO                   PIC X(10).
           05  RE-MOD                  PIC X(6).
           05  RE-SEV                  PIC X(12).
           05  RE-CAT                  PIC X(24).
           05  RE-MSG                  PIC X(60).
           05  RE-STAT                 PIC X(14).
           05  RE-AGE                  PIC X(8).
       01  R-PER.
           05  RP-CO                   PIC X(10).
           05  RP-YR                   PIC X(6).
           05  RP-NO                   PIC X(4).
           05  RP-NAME                 PIC X(20).
           05  RP-STAT                 PIC X(8).
           05  RP-FROM                 PIC X(10).
           05  RP-TO                   PIC X(10).
           05  RP-CLOSED               PIC X(20).
       01  R-ONE                       PIC X(120).
      *>================================================================
       SCREEN SECTION.
       01  SCR-CLEAR.
           05  BLANK SCREEN.
      *>   sign on panel - carries the product name in full
       01  SCR-SIGNON.
           05  BLANK SCREEN.
           05  LINE 2 COL 22 VALUE "STERP - Stefan's ERP".
           05  LINE 4 COL 22 VALUE "STELLAR COMPONENTS GROUP PLC".
           05  LINE 6 COL 22 VALUE "OPERATOR CONSOLE  STERPOPS".
           05  LINE 8 COL 2  PIC X(78) FROM WS-RULE.
           05  LINE 10 COL 22 VALUE "USER CODE ==>".
           05  LINE 10 COL 36 PIC X(20) USING WS-IN-USER AUTO.
           05  LINE 12 COL 22 VALUE "COMPANY   ==>".
           05  LINE 12 COL 36 PIC X(10) USING WS-IN-CO AUTO.
           05  LINE 14 COL 22 VALUE "(COMPANY BLANK = USER DEFAULT)".
           05  LINE 20 COL 2  PIC X(78) FROM WS-RULE.
           05  LINE 21 COL 2  PIC X(78) FROM WS-MSG.
           05  LINE 23 COL 2  VALUE
               "ENTER=SIGN ON  F3=EXIT  (X IN USER CODE = EXIT)".
      *>   the standard 24 x 80 frame used by every list panel
       01  SCR-FRAME.
           05  BLANK SCREEN.
           05  LINE 1 COL 2  VALUE "STERP".
           05  LINE 1 COL 9  PIC X(44) FROM WS-HDR-TITLE.
           05  LINE 1 COL 56 PIC X(19) FROM WS-HDR-DATE.
           05  LINE 2 COL 2  VALUE "USER".
           05  LINE 2 COL 7  PIC X(20) FROM WS-USER.
           05  LINE 2 COL 28 VALUE "CO".
           05  LINE 2 COL 31 PIC X(10) FROM WS-CO.
           05  LINE 2 COL 44 PIC X(24) FROM WS-PAGE-TXT.
           05  LINE 3 COL 2  PIC X(78) FROM WS-RULE.
           05  LINE 5 COL 2  PIC X(78) FROM WS-DET-LINE(1).
           05  LINE 6 COL 2  PIC X(78) FROM WS-DET-LINE(2).
           05  LINE 7 COL 2  PIC X(78) FROM WS-DET-LINE(3).
           05  LINE 8 COL 2  PIC X(78) FROM WS-DET-LINE(4).
           05  LINE 9 COL 2  PIC X(78) FROM WS-DET-LINE(5).
           05  LINE 10 COL 2 PIC X(78) FROM WS-DET-LINE(6).
           05  LINE 11 COL 2 PIC X(78) FROM WS-DET-LINE(7).
           05  LINE 12 COL 2 PIC X(78) FROM WS-DET-LINE(8).
           05  LINE 13 COL 2 PIC X(78) FROM WS-DET-LINE(9).
           05  LINE 14 COL 2 PIC X(78) FROM WS-DET-LINE(10).
           05  LINE 15 COL 2 PIC X(78) FROM WS-DET-LINE(11).
           05  LINE 16 COL 2 PIC X(78) FROM WS-DET-LINE(12).
           05  LINE 17 COL 2 PIC X(78) FROM WS-DET-LINE(13).
           05  LINE 18 COL 2 PIC X(78) FROM WS-DET-LINE(14).
           05  LINE 19 COL 2 PIC X(78) FROM WS-DET-LINE(15).
           05  LINE 20 COL 2 PIC X(78) FROM WS-RULE.
           05  LINE 21 COL 2 PIC X(78) FROM WS-MSG.
           05  LINE 22 COL 2 PIC X(66) FROM WS-LEGEND.
       01  SCR-CMD.
           05  LINE 23 COL 2 VALUE "OPTION ===>".
           05  LINE 23 COL 14 PIC X(8) USING WS-OPT AUTO.
      *>   KEY=VALUE prompt panel for a submitted job
       01  SCR-PARMS.
           05  BLANK SCREEN.
           05  LINE 1 COL 2 VALUE "STERP  SUBMIT BATCH JOB".
           05  LINE 1 COL 56 PIC X(19) FROM WS-HDR-DATE.
           05  LINE 2 COL 2 PIC X(78) FROM WS-DET-LINE(1).
           05  LINE 3 COL 2 PIC X(78) FROM WS-RULE.
           05  LINE 5 COL 4 PIC X(14) FROM WS-PK(1).
           05  LINE 5 COL 20 VALUE "=".
           05  LINE 5 COL 22 PIC X(24) USING WS-PV(1).
           05  LINE 6 COL 4 PIC X(14) FROM WS-PK(2).
           05  LINE 6 COL 20 VALUE "=".
           05  LINE 6 COL 22 PIC X(24) USING WS-PV(2).
           05  LINE 7 COL 4 PIC X(14) FROM WS-PK(3).
           05  LINE 7 COL 20 VALUE "=".
           05  LINE 7 COL 22 PIC X(24) USING WS-PV(3).
           05  LINE 8 COL 4 PIC X(14) FROM WS-PK(4).
           05  LINE 8 COL 20 VALUE "=".
           05  LINE 8 COL 22 PIC X(24) USING WS-PV(4).
           05  LINE 9 COL 4 PIC X(14) FROM WS-PK(5).
           05  LINE 9 COL 20 VALUE "=".
           05  LINE 9 COL 22 PIC X(24) USING WS-PV(5).
           05  LINE 10 COL 4 PIC X(14) FROM WS-PK(6).
           05  LINE 10 COL 20 VALUE "=".
           05  LINE 10 COL 22 PIC X(24) USING WS-PV(6).
           05  LINE 11 COL 4 PIC X(14) FROM WS-PK(7).
           05  LINE 11 COL 20 VALUE "=".
           05  LINE 11 COL 22 PIC X(24) USING WS-PV(7).
           05  LINE 12 COL 4 PIC X(14) FROM WS-PK(8).
           05  LINE 12 COL 20 VALUE "=".
           05  LINE 12 COL 22 PIC X(24) USING WS-PV(8).
           05  LINE 20 COL 2 PIC X(78) FROM WS-RULE.
           05  LINE 21 COL 2 PIC X(78) FROM WS-MSG.
           05  LINE 22 COL 2 VALUE
               "ENTER=SUBMIT   F3=CANCEL (TYPE X IN THE FIRST VALUE)".
      *>================================================================
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-START.
           PERFORM 0050-INIT THRU 0050-EXIT
           IF SMOKE-RUN
               PERFORM 8000-SMOKE THRU 8000-EXIT
               MOVE WS-RC TO RETURN-CODE
               STOP RUN
           END-IF
           PERFORM 1000-SIGN-ON THRU 1000-EXIT
           IF WS-QUIT NOT = "Y"
               PERFORM 2000-MAIN-MENU THRU 2000-EXIT
           END-IF
           PERFORM 0900-CLOSE-DOWN THRU 0900-EXIT
           MOVE WS-RC TO RETURN-CODE
           STOP RUN.

      *>----------------------------------------------------------------
      *> REQ: COM-018  the console takes the same KEY=VALUE arguments as
      *> every batch program; SMOKE is the unattended self test
      *>----------------------------------------------------------------
       0050-INIT.
           MOVE FUNCTION CURRENT-DATE(1:16) TO WS-NOW
           COMPUTE WS-TODAY = WS-NOW / 100000000
           STRING WS-NOW-YYYY WS-NOW-MM WS-NOW-DD
                  WS-NOW-HH WS-NOW-MI WS-NOW-SS
               DELIMITED BY SIZE INTO WS-STAMP-14
           STRING WS-NOW-DD "/" WS-NOW-MM "/" WS-NOW-YYYY
                  "  " WS-NOW-HH ":" WS-NOW-MI
               DELIMITED BY SIZE INTO WS-HDR-DATE
           ACCEPT WS-CMD-LINE FROM COMMAND-LINE
           MOVE FUNCTION UPPER-CASE(WS-CMD-LINE) TO WS-CMD-LINE
           IF WS-CMD-LINE(1:6) = "SMOKE " OR WS-CMD-LINE = "SMOKE"
              OR WS-CMD-LINE(1:7) = "-SMOKE " OR WS-CMD-LINE = "-SMOKE"
               MOVE "Y" TO WS-SMOKE
           END-IF
           MOVE "USER" TO WS-ARG-KEY
           PERFORM 0070-GET-ARG THRU 0070-EXIT
           MOVE WS-ARG-VAL TO WS-USER
           MOVE "CO" TO WS-ARG-KEY
           PERFORM 0070-GET-ARG THRU 0070-EXIT
           MOVE WS-ARG-VAL TO WS-CO
           ACCEPT WS-HOME FROM ENVIRONMENT "STERP_HOME"
           IF WS-HOME = SPACES
               MOVE "." TO WS-HOME
           END-IF
           PERFORM 0080-CONNECT THRU 0080-EXIT.
       0050-EXIT.
           EXIT.

      *>   crude KEY=VALUE scan of the command line (no CMDARGS here -
      *>   the console must start even when the engine is unreachable)
       0070-GET-ARG.
           MOVE SPACES TO WS-ARG-VAL
           MOVE 0 TO WS-POS
           MOVE SPACES TO WS-TEXT
           STRING FUNCTION TRIM(WS-ARG-KEY) "=" DELIMITED BY SIZE
               INTO WS-TEXT
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-TEXT)) TO WS-LEN
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > 160 - WS-LEN OR WS-POS > 0
               IF WS-CMD-LINE(WS-I:WS-LEN) = WS-TEXT(1:WS-LEN)
                   IF WS-I = 1
                       COMPUTE WS-POS = WS-I + WS-LEN
                   ELSE
                       IF WS-CMD-LINE(WS-I - 1:1) = SPACE
                           COMPUTE WS-POS = WS-I + WS-LEN
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           IF WS-POS > 0
               UNSTRING WS-CMD-LINE(WS-POS:160 - WS-POS + 1)
                   DELIMITED BY SPACE INTO WS-ARG-VAL
           END-IF.
       0070-EXIT.
           EXIT.

       0080-CONNECT.
           MOVE SPACES TO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           SET SQLIO-CONNECT TO TRUE
           CALL "SQLIO" USING SQLIO-CA R-ONE
           IF SQLIO-SQLCODE < 0
               MOVE "N" TO WS-DB-OK
               MOVE SPACES TO WS-MSG
               STRING "STERP-E-9001 DATABASE UNAVAILABLE - "
                      SQLIO-ERRMSG(1:40) DELIMITED BY SIZE INTO WS-MSG
               IF SMOKE-RUN
                   DISPLAY FUNCTION TRIM(WS-MSG)
                   MOVE 12 TO WS-RC
               END-IF
           ELSE
               MOVE "Y" TO WS-DB-OK
           END-IF.
       0080-EXIT.
           EXIT.

       0900-CLOSE-DOWN.
           IF WS-DB-OK = "Y"
               SET SQLIO-DISCONNECT TO TRUE
               CALL "SQLIO" USING SQLIO-CA R-ONE
           END-IF
           IF NOT SMOKE-RUN
               DISPLAY SCR-CLEAR
               DISPLAY "STERP-I-0099 OPERATOR CONSOLE ENDED"
           END-IF.
       0900-EXIT.
           EXIT.

      *>================================================================
      *> 1000  SIGN ON - REQ: COM-002  the user code must exist and be
      *> active in core.AppUser; three failures end the session
      *>================================================================
       1000-SIGN-ON.
           MOVE 0 TO WS-ATTEMPT
           MOVE WS-USER TO WS-IN-USER
           MOVE WS-CO TO WS-IN-CO
           MOVE "SIGN ON REQUIRED" TO WS-MSG
           PERFORM UNTIL WS-QUIT = "Y" OR WS-USER-NAME NOT = SPACES
               DISPLAY SCR-SIGNON
               ACCEPT SCR-SIGNON
               IF K-F3 OR K-ESCAPE
                   MOVE "Y" TO WS-QUIT
                   EXIT PERFORM
               END-IF
               MOVE FUNCTION UPPER-CASE(WS-IN-USER) TO WS-IN-USER
               MOVE FUNCTION UPPER-CASE(WS-IN-CO) TO WS-IN-CO
               IF WS-IN-USER = "X" OR WS-IN-USER = "QUIT"
                   MOVE "Y" TO WS-QUIT
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-ATTEMPT
               PERFORM 1100-CHECK-USER THRU 1100-EXIT
               IF WS-USER-NAME = SPACES AND WS-ATTEMPT >= 3
                   MOVE "STERP-E-1003 SIGN ON REFUSED - CONSOLE LOCKED"
                       TO WS-MSG
                   DISPLAY SCR-SIGNON
                   MOVE 8 TO WS-RC
                   MOVE "Y" TO WS-QUIT
               END-IF
           END-PERFORM.
       1000-EXIT.
           EXIT.

       1100-CHECK-USER.
           MOVE SPACES TO WS-USER-NAME
           IF WS-DB-OK NOT = "Y"
               MOVE "STERP-E-1009 NO DATABASE - SIGN ON IMPOSSIBLE"
                   TO WS-MSG
               GO TO 1100-EXIT
           END-IF
           IF WS-IN-USER = SPACES
               MOVE "STERP-E-1001 USER CODE IS REQUIRED" TO WS-MSG
               GO TO 1100-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT DisplayName, ISNULL(DefaultCompanyCode,"
                  "''), ISNULL(DefaultSiteCode,''), CONVERT(char(1),"
                  "IsReadOnly) FROM core.AppUser WHERE UserCode = ? "
                  "AND IsActive = 1"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-IN-USER TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 10 TO SQLIO-COL-WIDTH(2)
           MOVE 10 TO SQLIO-COL-WIDTH(3)
           MOVE 2 TO SQLIO-COL-WIDTH(4)
           MOVE 1 TO SQLIO-CURSOR
           MOVE SPACES TO R-USER
           SET SQLIO-SINGLE TO TRUE
           CALL "SQLIO" USING SQLIO-CA R-USER
           EVALUATE TRUE
               WHEN SQLIO-SQLCODE < 0
                   MOVE SPACES TO WS-MSG
                   STRING "STERP-E-1008 " SQLIO-ERRMSG(1:60)
                       DELIMITED BY SIZE INTO WS-MSG
               WHEN SQLIO-SQLCODE = 100
                   MOVE "STERP-E-1002 UNKNOWN OR INACTIVE USER CODE"
                       TO WS-MSG
               WHEN OTHER
                   MOVE WS-IN-USER TO WS-USER
                   MOVE RU-NAME TO WS-USER-NAME
                   IF WS-IN-CO NOT = SPACES
                       MOVE WS-IN-CO TO WS-CO
                   ELSE
                       MOVE RU-CO TO WS-CO
                   END-IF
                   MOVE RU-SITE TO WS-SITE
                   IF RU-RDONLY(1:1) = "1"
                       MOVE "Y" TO WS-RDONLY
                   ELSE
                       MOVE "N" TO WS-RDONLY
                   END-IF
                   MOVE SPACES TO WS-MSG
                   STRING "STERP-I-1000 WELCOME "
                          FUNCTION TRIM(WS-USER-NAME)
                       DELIMITED BY SIZE INTO WS-MSG
           END-EVALUATE.
       1100-EXIT.
           EXIT.

      *>================================================================
      *> 2000  MAIN MENU
      *>================================================================
       2000-MAIN-MENU.
           PERFORM UNTIL WS-QUIT = "Y"
               PERFORM 9200-CLEAR-DET THRU 9200-EXIT
               MOVE "   1   SUBMIT BATCH JOB / JOB STREAM"
                   TO WS-DET-LINE(2)
               MOVE "   2   JOB MONITOR          (core.JobRun)"
                   TO WS-DET-LINE(4)
               MOVE "   3   EXCEPTION QUEUE      (core.ExceptionItem)"
                   TO WS-DET-LINE(6)
               MOVE "   4   PERIOD STATUS        (fin.Period)"
                   TO WS-DET-LINE(8)
               MOVE "   5   ENGINE HEALTH        (PING)"
                   TO WS-DET-LINE(10)
               MOVE "   X   SIGN OFF"
                   TO WS-DET-LINE(12)
               MOVE "OPERATIONS MAIN MENU" TO WS-HDR-TITLE
               MOVE SPACES TO WS-PAGE-TXT
               MOVE "F3=SIGN OFF   ENTER OPTION AND PRESS ENTER"
                   TO WS-LEGEND
               PERFORM 9300-SHOW-PANEL THRU 9300-EXIT
               EVALUATE TRUE
                   WHEN K-F3 OR K-ESCAPE
                       MOVE "Y" TO WS-QUIT
                   WHEN WS-OPT = "X" OR WS-OPT = "END"
                       MOVE "Y" TO WS-QUIT
                   WHEN WS-OPT(1:1) = "1" OR WS-OPT(1:1) = "A"
                       PERFORM 3000-SUBMIT-JOB THRU 3000-EXIT
                   WHEN WS-OPT(1:1) = "2" OR WS-OPT(1:1) = "B"
                       PERFORM 4000-JOB-MONITOR THRU 4000-EXIT
                   WHEN WS-OPT(1:1) = "3" OR WS-OPT(1:1) = "C"
                       PERFORM 5000-EXCEPTIONS THRU 5000-EXIT
                   WHEN WS-OPT(1:1) = "4" OR WS-OPT(1:1) = "D"
                       PERFORM 6000-PERIOD-STATUS THRU 6000-EXIT
                   WHEN WS-OPT(1:1) = "5" OR WS-OPT(1:1) = "E"
                       PERFORM 7000-ENGINE-HEALTH THRU 7000-EXIT
                   WHEN WS-OPT = SPACES
                       CONTINUE
                   WHEN OTHER
                       MOVE "STERP-W-2001 INVALID OPTION" TO WS-MSG
               END-EVALUATE
           END-PERFORM.
       2000-EXIT.
           EXIT.

      *>================================================================
      *> 3000  SUBMIT BATCH JOB - REQ: XMD-005, RPT-015
      *> the catalogue is core.ReportDef; rows whose code starts JOB.
      *> are the JCL style job streams in jobs\*.cmd, everything else
      *> is a batch EXE in bin\x86
      *>================================================================
       3000-SUBMIT-JOB.
           PERFORM 3100-LOAD-JOBS THRU 3100-EXIT
           MOVE 1 TO WS-TOP
           MOVE "N" TO WS-LEAVE
           PERFORM UNTIL WS-LEAVE = "Y"
               MOVE "SUBMIT BATCH JOB - CATALOGUE" TO WS-HDR-TITLE
               MOVE "F3=MENU  F7=PREV(-)  F8=NEXT(+)  NUMBER=SUBMIT"
                   TO WS-LEGEND
               PERFORM 9400-PAGE-LINES THRU 9400-EXIT
               PERFORM 9300-SHOW-PANEL THRU 9300-EXIT
               PERFORM 9450-NAVIGATE THRU 9450-EXIT
               IF WS-SEL > 0 AND WS-SEL <= WS-ROW-CNT
                   PERFORM 3200-PROMPT-PARMS THRU 3200-EXIT
               END-IF
           END-PERFORM
           MOVE "N" TO WS-LEAVE.
       3000-EXIT.
           EXIT.

       3100-LOAD-JOBS.
           MOVE 0 TO WS-ROW-CNT
           MOVE 1 TO WS-TOP
           IF WS-DB-OK NOT = "Y"
               MOVE "STERP-E-3009 NO DATABASE CONNECTION" TO WS-MSG
               GO TO 3100-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ReportCode, Module, ProgramName, ISNULL("
                  "ParamSpec,''), LEFT(Title,40) FROM core.ReportDef "
                  "WHERE IsActive = 1 ORDER BY CASE WHEN ReportCode "
                  "LIKE 'JOB.%' THEN 0 ELSE 1 END, Module, ReportCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 6 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-COL-WIDTH(3)
           MOVE 100 TO SQLIO-COL-WIDTH(4)
           MOVE 40 TO SQLIO-COL-WIDTH(5)
           MOVE 2 TO SQLIO-CURSOR
           SET SQLIO-EXECUTE TO TRUE
           CALL "SQLIO" USING SQLIO-CA R-JOB
           IF SQLIO-SQLCODE < 0
               PERFORM 9600-SQL-ERROR THRU 9600-EXIT
               GO TO 3100-EXIT
           END-IF
           PERFORM UNTIL WS-ROW-CNT >= 300
               MOVE SPACES TO R-JOB
               SET SQLIO-FETCH TO TRUE
               CALL "SQLIO" USING SQLIO-CA R-JOB
               IF SQLIO-SQLCODE NOT = 0
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-ROW-CNT
               MOVE RJ-CODE TO WS-ROW-KEY(WS-ROW-CNT)
               MOVE SPACES TO WS-ROW-AUX(WS-ROW-CNT)
               MOVE RJ-PROG TO WS-ROW-AUX(WS-ROW-CNT)(1:8)
               MOVE RJ-PARMS TO WS-ROW-AUX(WS-ROW-CNT)(10:100)
               MOVE WS-ROW-CNT TO WS-SEQ-ED
               MOVE SPACES TO WS-ROW-TXT(WS-ROW-CNT)
               STRING WS-SEQ-ED " " RJ-CODE(1:20) " " RJ-MOD(1:4)
                      " " RJ-PROG(1:8) " " RJ-TITLE(1:38)
                   DELIMITED BY SIZE
                   INTO WS-ROW-TXT(WS-ROW-CNT)
           END-PERFORM
           SET SQLIO-CLOSE TO TRUE
           CALL "SQLIO" USING SQLIO-CA R-JOB
           MOVE SPACES TO WS-MSG
           MOVE WS-ROW-CNT TO WS-NUM-ED
           STRING "STERP-I-3000 " FUNCTION TRIM(WS-NUM-ED)
                  " RUNNABLE DEFINITIONS IN THE CATALOGUE"
               DELIMITED BY SIZE INTO WS-MSG.
       3100-EXIT.
           EXIT.

      *>   KEY=VALUE prompting.  ParamSpec is a comma list; an entry
      *>   written KEY=VALUE carries a fixed default (BOM/KIT use it)
       3200-PROMPT-PARMS.
           MOVE WS-ROW-KEY(WS-SEL) TO WS-JOB-CODE
           MOVE WS-ROW-AUX(WS-SEL)(1:8) TO WS-JOB-PROG
           MOVE WS-ROW-AUX(WS-SEL)(10:100) TO WS-PARM-SPEC
           IF WS-JOB-CODE(1:4) = "JOB."
               MOVE "Y" TO WS-JOB-STREAM
           ELSE
               MOVE "N" TO WS-JOB-STREAM
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8
               MOVE SPACES TO WS-PK(WS-I) WS-PV(WS-I)
           END-PERFORM
           MOVE 0 TO WS-K
           UNSTRING WS-PARM-SPEC DELIMITED BY ","
               INTO WS-PK(1) WS-PK(2) WS-PK(3) WS-PK(4)
                    WS-PK(5) WS-PK(6) WS-PK(7) WS-PK(8)
               TALLYING IN WS-K
           END-UNSTRING
           MOVE WS-K TO WS-PARM-CNT
           IF WS-PARM-CNT > 8
               MOVE 8 TO WS-PARM-CNT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-PARM-CNT
               MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(WS-PK(WS-I)))
                   TO WS-TEXT
               MOVE SPACES TO WS-PK(WS-I)
               UNSTRING WS-TEXT DELIMITED BY "="
                   INTO WS-PK(WS-I) WS-PV(WS-I)
               END-UNSTRING
               EVALUATE WS-PK(WS-I)
                   WHEN "CO"
                       MOVE WS-CO TO WS-PV(WS-I)
                   WHEN "USER"
                       MOVE WS-USER TO WS-PV(WS-I)
                   WHEN "SITE"
                       IF WS-PV(WS-I) = SPACES
                           MOVE WS-SITE TO WS-PV(WS-I)
                       END-IF
                   WHEN "ASAT"
                       IF WS-PV(WS-I) = SPACES
                           MOVE WS-TODAY TO WS-NUM
                           MOVE WS-NUM TO WS-NUM-ED
                           MOVE FUNCTION TRIM(WS-NUM-ED)
                               TO WS-PV(WS-I)
                       END-IF
                   WHEN "MODE"
                       IF WS-PV(WS-I) = SPACES
                           MOVE "PREVIEW" TO WS-PV(WS-I)
                       END-IF
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
           MOVE SPACES TO WS-DET-LINE(1)
           STRING "JOB " WS-JOB-CODE(1:20) " PROGRAM " WS-JOB-PROG
                  " - AMEND THE VALUES AND PRESS ENTER"
               DELIMITED BY SIZE INTO WS-DET-LINE(1)
           MOVE "STERP-I-3200 ENTER=SUBMIT  X IN FIRST VALUE=CANCEL"
               TO WS-MSG
           DISPLAY SCR-PARMS
           ACCEPT SCR-PARMS
           IF K-F3 OR K-ESCAPE
               GO TO 3200-EXIT
           END-IF
           IF FUNCTION UPPER-CASE(FUNCTION TRIM(WS-PV(1))) = "X"
               MOVE "STERP-I-3201 SUBMISSION CANCELLED" TO WS-MSG
               GO TO 3200-EXIT
           END-IF
           PERFORM 3300-RUN-JOB THRU 3300-EXIT.
       3200-EXIT.
           EXIT.

      *>   REQ: XMD-005  run the work and report the completion code
       3300-RUN-JOB.
           PERFORM 3350-BUILD-COMMAND THRU 3350-EXIT
           IF NOT SMOKE-RUN
               DISPLAY SCR-CLEAR
               DISPLAY "STERP-I-3300 SUBMITTED: "
                   FUNCTION TRIM(WS-SYS-CMD)
           END-IF
           MOVE 0 TO WS-JOB-RC
           CALL "SYSTEM" USING WS-SYS-CMD
           MOVE RETURN-CODE TO WS-JOB-RC
           MOVE 0 TO RETURN-CODE
           PERFORM 3400-LAST-RUN THRU 3400-EXIT
           MOVE WS-JOB-RC TO WS-RC-ED
           MOVE SPACES TO WS-MSG
           STRING "STERP-I-3301 " FUNCTION TRIM(WS-JOB-CODE)
                  " ENDED RC=" FUNCTION TRIM(WS-RC-ED)
                  " STATUS=" FUNCTION TRIM(WS-JOB-STAT)
                  " SPOOL=" FUNCTION TRIM(WS-JOB-SPOOL)
               DELIMITED BY SIZE INTO WS-MSG
           IF NOT SMOKE-RUN
               MOVE "JOB COMPLETION" TO WS-HDR-TITLE
               MOVE SPACES TO WS-PAGE-TXT WS-OPT
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 15
                   MOVE SPACES TO WS-DET-LINE(WS-I)
               END-PERFORM
               MOVE WS-MSG TO WS-DET-LINE(1)
               MOVE "SPOOL / LOG:" TO WS-DET-LINE(3)
               MOVE WS-JOB-SPOOL(1:78) TO WS-DET-LINE(4)
               MOVE WS-JOB-SPOOL(79:32) TO WS-DET-LINE(5)
               MOVE "ENTER=RETURN TO THE CATALOGUE" TO WS-LEGEND
               DISPLAY SCR-FRAME
               ACCEPT SCR-CMD
           END-IF.
       3300-EXIT.
           EXIT.

       3350-BUILD-COMMAND.
           MOVE SPACES TO WS-SYS-CMD
           MOVE SPACES TO WS-TEXT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-PARM-CNT
               IF WS-PV(WS-I) NOT = SPACES AND WS-PK(WS-I) NOT = SPACES
                   STRING FUNCTION TRIM(WS-TEXT) " "
                          FUNCTION TRIM(WS-PK(WS-I)) "="
                          FUNCTION TRIM(WS-PV(WS-I))
                       DELIMITED BY SIZE INTO WS-TEXT
               END-IF
           END-PERFORM
           IF WS-JOB-STREAM = "Y"
               STRING "cd /d """ FUNCTION TRIM(WS-HOME)
                      """ && call jobs\" FUNCTION TRIM(WS-JOB-PROG)
                      ".cmd " FUNCTION TRIM(WS-TEXT)
                   DELIMITED BY SIZE INTO WS-SYS-CMD
           ELSE
               STRING "cd /d """ FUNCTION TRIM(WS-HOME)
                      """ && bin\x86\" FUNCTION TRIM(WS-JOB-PROG)
                      ".exe " FUNCTION TRIM(WS-TEXT)
                      " > run\logs\OPS-" FUNCTION TRIM(WS-JOB-PROG)
                      "-" WS-STAMP-14 ".log 2>&1"
                   DELIMITED BY SIZE INTO WS-SYS-CMD
           END-IF.
       3350-EXIT.
           EXIT.

       3400-LAST-RUN.
           MOVE SPACES TO WS-JOB-SPOOL
           MOVE "UNKNOWN" TO WS-JOB-STAT
           IF WS-DB-OK NOT = "Y"
               GO TO 3400-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 Status, ISNULL(SpoolFile,ISNULL("
                  "LogFile,'')) FROM core.JobRun WHERE JobName = ? "
                  "ORDER BY JobRunId DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-JOB-PROG TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 108 TO SQLIO-COL-WIDTH(2)
           MOVE 3 TO SQLIO-CURSOR
           MOVE SPACES TO R-ONE
           SET SQLIO-SINGLE TO TRUE
           CALL "SQLIO" USING SQLIO-CA R-ONE
           IF SQLIO-SQLCODE = 0
               MOVE R-ONE(1:12) TO WS-JOB-STAT
               MOVE R-ONE(13:108) TO WS-JOB-SPOOL
           END-IF.
       3400-EXIT.
           EXIT.

      *>================================================================
      *> 4000  JOB MONITOR - REQ: XMD-007, COM-018
      *>================================================================
       4000-JOB-MONITOR.
           PERFORM 4100-LOAD-RUNS THRU 4100-EXIT
           MOVE "N" TO WS-LEAVE
           PERFORM UNTIL WS-LEAVE = "Y"
               MOVE "JOB MONITOR - LATEST RUNS FIRST" TO WS-HDR-TITLE
               MOVE "F3=MENU  F7=PREV(-)  F8=NEXT(+)  ENTER=REFRESH"
                   TO WS-LEGEND
               PERFORM 9400-PAGE-LINES THRU 9400-EXIT
               PERFORM 9300-SHOW-PANEL THRU 9300-EXIT
               PERFORM 9450-NAVIGATE THRU 9450-EXIT
               IF WS-OPT = SPACES AND WS-LEAVE = "N"
                   PERFORM 4100-LOAD-RUNS THRU 4100-EXIT
               END-IF
               IF WS-SEL > 0 AND WS-SEL <= WS-ROW-CNT
                   MOVE SPACES TO WS-MSG
                   STRING "SPOOL/LOG: "
                          WS-ROW-AUX(WS-SEL)(1:66)
                       DELIMITED BY SIZE INTO WS-MSG
               END-IF
           END-PERFORM
           MOVE "N" TO WS-LEAVE.
       4000-EXIT.
           EXIT.

       4100-LOAD-RUNS.
           MOVE 0 TO WS-ROW-CNT
           MOVE 1 TO WS-TOP
           IF WS-DB-OK NOT = "Y"
               MOVE "STERP-E-4009 NO DATABASE CONNECTION" TO WS-MSG
               GO TO 4100-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 300 CONVERT(varchar(12),JobRunId), "
                  "JobName, ISNULL(CompanyCode,''), CONVERT(varchar"
                  "(19),StartedAt,120), Status, ISNULL(CONVERT("
                  "varchar(8),ReturnCode),''), ISNULL(SpoolFile,"
                  "ISNULL(LogFile,'')) FROM core.JobRun ORDER BY "
                  "JobRunId DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 7 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 10 TO SQLIO-COL-WIDTH(3)
           MOVE 20 TO SQLIO-COL-WIDTH(4)
           MOVE 12 TO SQLIO-COL-WIDTH(5)
           MOVE 8 TO SQLIO-COL-WIDTH(6)
           MOVE 110 TO SQLIO-COL-WIDTH(7)
           MOVE 2 TO SQLIO-CURSOR
           SET SQLIO-EXECUTE TO TRUE
           CALL "SQLIO" USING SQLIO-CA R-RUN
           IF SQLIO-SQLCODE < 0
               PERFORM 9600-SQL-ERROR THRU 9600-EXIT
               GO TO 4100-EXIT
           END-IF
           PERFORM UNTIL WS-ROW-CNT >= 300
               MOVE SPACES TO R-RUN
               SET SQLIO-FETCH TO TRUE
               CALL "SQLIO" USING SQLIO-CA R-RUN
               IF SQLIO-SQLCODE NOT = 0
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-ROW-CNT
               MOVE RR-ID TO WS-ROW-KEY(WS-ROW-CNT)
               MOVE RR-SPOOL TO WS-ROW-AUX(WS-ROW-CNT)
               MOVE WS-ROW-CNT TO WS-SEQ-ED
               MOVE SPACES TO WS-ROW-TXT(WS-ROW-CNT)
               STRING WS-SEQ-ED " " RR-ID(1:7) " " RR-NAME(1:20)
                      " " RR-CO(1:4) " " RR-START(1:16)
                      " " RR-STAT(1:9) " " RR-RC(1:4)
                   DELIMITED BY SIZE
                   INTO WS-ROW-TXT(WS-ROW-CNT)
           END-PERFORM
           SET SQLIO-CLOSE TO TRUE
           CALL "SQLIO" USING SQLIO-CA R-RUN
           MOVE WS-ROW-CNT TO WS-NUM-ED
           MOVE SPACES TO WS-MSG
           STRING "STERP-I-4000 " FUNCTION TRIM(WS-NUM-ED)
                  " RUNS - NUM SHOWS THE SPOOL FILE"
               DELIMITED BY SIZE INTO WS-MSG.
       4100-EXIT.
           EXIT.

      *>================================================================
      *> 5000  EXCEPTION QUEUE - REQ: COM-017, XMD-007
      *>================================================================
       5000-EXCEPTIONS.
           PERFORM 5100-LOAD-EXC THRU 5100-EXIT
           MOVE "N" TO WS-LEAVE
           PERFORM UNTIL WS-LEAVE = "Y"
               MOVE "EXCEPTION QUEUE - OPEN AND ACKNOWLEDGED"
                   TO WS-HDR-TITLE
               MOVE "F3=MENU  F7/F8=PAGE  NUMBER=ACKNOWLEDGE"
                   TO WS-LEGEND
               PERFORM 9400-PAGE-LINES THRU 9400-EXIT
               PERFORM 9300-SHOW-PANEL THRU 9300-EXIT
               PERFORM 9450-NAVIGATE THRU 9450-EXIT
               IF WS-SEL > 0 AND WS-SEL <= WS-ROW-CNT
                   PERFORM 5200-ACKNOWLEDGE THRU 5200-EXIT
               END-IF
           END-PERFORM
           MOVE "N" TO WS-LEAVE.
       5000-EXIT.
           EXIT.

       5100-LOAD-EXC.
           MOVE 0 TO WS-ROW-CNT
           MOVE 1 TO WS-TOP
           IF WS-DB-OK NOT = "Y"
               MOVE "STERP-E-5009 NO DATABASE CONNECTION" TO WS-MSG
               GO TO 5100-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),ExceptionId), "
                  "CompanyCode, Module, Severity, LEFT(Category,24),"
                  " LEFT(Message,60), Status, ISNULL(CONVERT("
                  "varchar(8),AgeDays),'') FROM core.ExceptionItem "
                  "WHERE Status <> 'RESOLVED' ORDER BY Priority, "
                  "ExceptionId"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 8 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 10 TO SQLIO-COL-WIDTH(2)
           MOVE 6 TO SQLIO-COL-WIDTH(3)
           MOVE 12 TO SQLIO-COL-WIDTH(4)
           MOVE 24 TO SQLIO-COL-WIDTH(5)
           MOVE 60 TO SQLIO-COL-WIDTH(6)
           MOVE 14 TO SQLIO-COL-WIDTH(7)
           MOVE 8 TO SQLIO-COL-WIDTH(8)
           MOVE 2 TO SQLIO-CURSOR
           SET SQLIO-EXECUTE TO TRUE
           CALL "SQLIO" USING SQLIO-CA R-EXC
           IF SQLIO-SQLCODE < 0
               PERFORM 9600-SQL-ERROR THRU 9600-EXIT
               GO TO 5100-EXIT
           END-IF
           PERFORM UNTIL WS-ROW-CNT >= 300
               MOVE SPACES TO R-EXC
               SET SQLIO-FETCH TO TRUE
               CALL "SQLIO" USING SQLIO-CA R-EXC
               IF SQLIO-SQLCODE NOT = 0
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-ROW-CNT
               MOVE RE-ID TO WS-ROW-KEY(WS-ROW-CNT)
               MOVE RE-MSG TO WS-ROW-AUX(WS-ROW-CNT)
               MOVE WS-ROW-CNT TO WS-SEQ-ED
               MOVE SPACES TO WS-ROW-TXT(WS-ROW-CNT)
               STRING WS-SEQ-ED " " RE-ID(1:6) " " RE-CO(1:4)
                      " " RE-MOD(1:4) " " RE-SEV(1:7)
                      " " RE-CAT(1:16) " " RE-MSG(1:24)
                      " " RE-STAT(1:5)
                   DELIMITED BY SIZE
                   INTO WS-ROW-TXT(WS-ROW-CNT)
           END-PERFORM
           SET SQLIO-CLOSE TO TRUE
           CALL "SQLIO" USING SQLIO-CA R-EXC
           MOVE WS-ROW-CNT TO WS-NUM-ED
           MOVE SPACES TO WS-MSG
           STRING "STERP-I-5000 " FUNCTION TRIM(WS-NUM-ED)
                  " OPEN EXCEPTIONS"
               DELIMITED BY SIZE INTO WS-MSG.
       5100-EXIT.
           EXIT.

      *>   acknowledging is an ordinary work list action, not a
      *>   resolution - core.usp_ResolveException owns the rules
       5200-ACKNOWLEDGE.
           IF WS-RDONLY = "Y"
               MOVE "STERP-E-5002 READ ONLY USER MAY NOT ACKNOWLEDGE"
                   TO WS-MSG
               GO TO 5200-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           MOVE "EXEC core.usp_ResolveException ?, ?, ?, ?"
               TO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE WS-ROW-KEY(WS-SEL) TO SQLIO-PARM-VAL(1)
           MOVE WS-USER TO SQLIO-PARM-VAL(2)
           MOVE "ACKNOWLEDGED AT THE OPERATOR CONSOLE"
               TO SQLIO-PARM-VAL(3)
           MOVE "ACKNOWLEDGED" TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           SET SQLIO-EXECUTE TO TRUE
           CALL "SQLIO" USING SQLIO-CA R-ONE
           IF SQLIO-SQLCODE < 0
               PERFORM 9600-SQL-ERROR THRU 9600-EXIT
           ELSE
               SET SQLIO-CLOSE TO TRUE
               CALL "SQLIO" USING SQLIO-CA R-ONE
               MOVE SPACES TO WS-MSG
               STRING "STERP-I-5001 EXCEPTION "
                      FUNCTION TRIM(WS-ROW-KEY(WS-SEL))
                      " ACKNOWLEDGED BY " FUNCTION TRIM(WS-USER)
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 5100-LOAD-EXC THRU 5100-EXIT
           END-IF.
       5200-EXIT.
           EXIT.

      *>================================================================
      *> 6000  PERIOD STATUS PER COMPANY - REQ: COM-009, XMD-007
      *>================================================================
       6000-PERIOD-STATUS.
           PERFORM 6100-LOAD-PERIODS THRU 6100-EXIT
           MOVE "N" TO WS-LEAVE
           PERFORM UNTIL WS-LEAVE = "Y"
               MOVE "LEDGER PERIOD STATUS BY COMPANY" TO WS-HDR-TITLE
               MOVE "F3=MENU  F7/F8=PAGE  ENTER=REFRESH"
                   TO WS-LEGEND
               PERFORM 9400-PAGE-LINES THRU 9400-EXIT
               PERFORM 9300-SHOW-PANEL THRU 9300-EXIT
               PERFORM 9450-NAVIGATE THRU 9450-EXIT
               IF WS-OPT = SPACES AND WS-LEAVE = "N"
                   PERFORM 6100-LOAD-PERIODS THRU 6100-EXIT
               END-IF
           END-PERFORM
           MOVE "N" TO WS-LEAVE.
       6000-EXIT.
           EXIT.

       6100-LOAD-PERIODS.
           MOVE 0 TO WS-ROW-CNT
           MOVE 1 TO WS-TOP
           IF WS-DB-OK NOT = "Y"
               MOVE "STERP-E-6009 NO DATABASE CONNECTION" TO WS-MSG
               GO TO 6100-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CompanyCode, CONVERT(varchar(6),FiscalYear"
                  "), CONVERT(varchar(4),PeriodNo), Name, Status, "
                  "CONVERT(char(10),StartDate,103), CONVERT(char(10)"
                  ",EndDate,103), ISNULL(ClosedBy,'') FROM fin.Period"
                  " WHERE FiscalYear >= YEAR(GETDATE()) - 1 ORDER BY"
                  " CompanyCode, FiscalYear, PeriodNo"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 8 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 6 TO SQLIO-COL-WIDTH(2)
           MOVE 4 TO SQLIO-COL-WIDTH(3)
           MOVE 20 TO SQLIO-COL-WIDTH(4)
           MOVE 8 TO SQLIO-COL-WIDTH(5)
           MOVE 10 TO SQLIO-COL-WIDTH(6)
           MOVE 10 TO SQLIO-COL-WIDTH(7)
           MOVE 20 TO SQLIO-COL-WIDTH(8)
           MOVE 2 TO SQLIO-CURSOR
           SET SQLIO-EXECUTE TO TRUE
           CALL "SQLIO" USING SQLIO-CA R-PER
           IF SQLIO-SQLCODE < 0
               PERFORM 9600-SQL-ERROR THRU 9600-EXIT
               GO TO 6100-EXIT
           END-IF
           PERFORM UNTIL WS-ROW-CNT >= 300
               MOVE SPACES TO R-PER
               SET SQLIO-FETCH TO TRUE
               CALL "SQLIO" USING SQLIO-CA R-PER
               IF SQLIO-SQLCODE NOT = 0
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-ROW-CNT
               MOVE RP-CO TO WS-ROW-KEY(WS-ROW-CNT)
               MOVE WS-ROW-CNT TO WS-SEQ-ED
               MOVE SPACES TO WS-ROW-TXT(WS-ROW-CNT)
               STRING WS-SEQ-ED " " RP-CO(1:5) " " RP-YR(1:4)
                      " P" RP-NO(1:3) " " RP-NAME(1:12)
                      " " RP-STAT(1:6) " " RP-FROM(1:10)
                      " " RP-TO(1:10) " " RP-CLOSED(1:12)
                   DELIMITED BY SIZE
                   INTO WS-ROW-TXT(WS-ROW-CNT)
           END-PERFORM
           SET SQLIO-CLOSE TO TRUE
           CALL "SQLIO" USING SQLIO-CA R-PER
           MOVE WS-ROW-CNT TO WS-NUM-ED
           MOVE SPACES TO WS-MSG
           STRING "STERP-I-6000 " FUNCTION TRIM(WS-NUM-ED)
                  " PERIODS - FUTURE/OPEN/SOFT/HARD"
               DELIMITED BY SIZE INTO WS-MSG.
       6100-EXIT.
           EXIT.

      *>================================================================
      *> 7000  ENGINE HEALTH - REQ: XMD-007, COM-018
      *>================================================================
       7000-ENGINE-HEALTH.
           PERFORM 7100-PING THRU 7100-EXIT
           MOVE "N" TO WS-LEAVE
           PERFORM UNTIL WS-LEAVE = "Y"
               MOVE "ENGINE AND DATABASE HEALTH" TO WS-HDR-TITLE
               MOVE SPACES TO WS-PAGE-TXT
               MOVE "F3=MENU  ENTER=PING AGAIN" TO WS-LEGEND
               PERFORM 9300-SHOW-PANEL THRU 9300-EXIT
               PERFORM 9450-NAVIGATE THRU 9450-EXIT
               IF WS-LEAVE = "N"
                   PERFORM 7100-PING THRU 7100-EXIT
               END-IF
           END-PERFORM
           MOVE "N" TO WS-LEAVE.
       7000-EXIT.
           EXIT.

       7100-PING.
           PERFORM 9200-CLEAR-DET THRU 9200-EXIT
           INITIALIZE WS-PING-CA
           MOVE "PING" TO PC-PROGRAM
           MOVE "HEALTH" TO PC-FUNCTION
           MOVE WS-CO TO PC-COMPANY
           MOVE WS-SITE TO PC-SITE
           MOVE WS-USER TO PC-USER
           MOVE WS-TODAY TO PC-BUSINESS-DATE
           MOVE 1234.56 TO PG-ECHO-AMOUNT
           CALL "PING" USING WS-PING-CA
           MOVE SPACES TO WS-DET-LINE(1)
           STRING "ENGINE VERSION . " PG-ENGINE-VERSION
               DELIMITED BY SIZE INTO WS-DET-LINE(1)
           MOVE SPACES TO WS-DET-LINE(2)
           STRING "DATABASE . . . . " PG-DB-NAME
               DELIMITED BY SIZE INTO WS-DET-LINE(2)
           MOVE SPACES TO WS-DET-LINE(3)
           STRING "SERVER TIME  . . " PG-DB-SERVER-TIME
               DELIMITED BY SIZE INTO WS-DET-LINE(3)
           MOVE SPACES TO WS-DET-LINE(4)
           STRING "DB VERSION . . . " PG-DB-VERSION(1:55)
               DELIMITED BY SIZE INTO WS-DET-LINE(4)
           MOVE PC-RETURN-CODE TO WS-RC-ED
           MOVE SPACES TO WS-DET-LINE(5)
           STRING "PING RETURN CODE " FUNCTION TRIM(WS-RC-ED)
                  "   REASON " PC-REASON-CODE
               DELIMITED BY SIZE INTO WS-DET-LINE(5)
           MOVE SPACES TO WS-DET-LINE(6)
           STRING "ECHO DOUBLED . . " PG-ECHO-DOUBLE
               DELIMITED BY SIZE INTO WS-DET-LINE(6)
           MOVE SPACES TO WS-DET-LINE(8)
           STRING "SQLIO CONNECTION " WS-DB-OK
                  "   CONSOLE HOME " FUNCTION TRIM(WS-HOME)
               DELIMITED BY SIZE INTO WS-DET-LINE(8)
           IF PC-RETURN-CODE = 0
               MOVE "STERP-I-7000 ENGINE AND DATABASE HEALTHY"
                   TO WS-MSG
           ELSE
               MOVE SPACES TO WS-MSG
               STRING "STERP-E-7001 " PC-MESSAGE(1:60)
                   DELIMITED BY SIZE INTO WS-MSG
           END-IF.
       7100-EXIT.
           EXIT.

      *>================================================================
      *> 8000  SMOKE TEST - every panel's data printed to SYSOUT so the
      *> console can be regression tested without a terminal
      *> REQ: XMD-005, XMD-007
      *>================================================================
       8000-SMOKE.
           DISPLAY "STERP-I-8000 STERPOPS SMOKE TEST "
                   FUNCTION TRIM(WS-HDR-DATE)
           IF WS-USER = SPACES
               MOVE "OPS" TO WS-IN-USER
           ELSE
               MOVE WS-USER TO WS-IN-USER
           END-IF
           MOVE WS-CO TO WS-IN-CO
           PERFORM 1100-CHECK-USER THRU 1100-EXIT
           IF WS-USER-NAME = SPACES
               DISPLAY "STERP-W-8001 " FUNCTION TRIM(WS-MSG)
               PERFORM 8100-ANY-USER THRU 8100-EXIT
           END-IF
           DISPLAY " "
           DISPLAY "--- SIGN ON ---------------------------------"
           DISPLAY "USER    " FUNCTION TRIM(WS-USER)
                   "  NAME " FUNCTION TRIM(WS-USER-NAME)
           DISPLAY "COMPANY " FUNCTION TRIM(WS-CO)
                   "  SITE " FUNCTION TRIM(WS-SITE)
                   "  READ ONLY " WS-RDONLY
           IF WS-USER-NAME = SPACES
               MOVE 8 TO WS-RC
           END-IF
           DISPLAY " "
           DISPLAY "--- 1 JOB CATALOGUE -------------------------"
           PERFORM 3100-LOAD-JOBS THRU 3100-EXIT
           MOVE 12 TO WS-K
           PERFORM 8200-DUMP-ROWS THRU 8200-EXIT
           DISPLAY " "
           DISPLAY "--- 2 JOB MONITOR ---------------------------"
           PERFORM 4100-LOAD-RUNS THRU 4100-EXIT
           MOVE 10 TO WS-K
           PERFORM 8200-DUMP-ROWS THRU 8200-EXIT
           DISPLAY " "
           DISPLAY "--- 3 EXCEPTION QUEUE -----------------------"
           PERFORM 5100-LOAD-EXC THRU 5100-EXIT
           MOVE 10 TO WS-K
           PERFORM 8200-DUMP-ROWS THRU 8200-EXIT
           DISPLAY " "
           DISPLAY "--- 4 PERIOD STATUS -------------------------"
           PERFORM 6100-LOAD-PERIODS THRU 6100-EXIT
           MOVE 12 TO WS-K
           PERFORM 8200-DUMP-ROWS THRU 8200-EXIT
           DISPLAY " "
           DISPLAY "--- 5 ENGINE HEALTH -------------------------"
           PERFORM 7100-PING THRU 7100-EXIT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8
               IF WS-DET-LINE(WS-I) NOT = SPACES
                   DISPLAY FUNCTION TRIM(WS-DET-LINE(WS-I))
               END-IF
           END-PERFORM
           DISPLAY FUNCTION TRIM(WS-MSG)
           IF PC-RETURN-CODE NOT = 0
               MOVE 8 TO WS-RC
           END-IF
           PERFORM 0900-CLOSE-DOWN THRU 0900-EXIT
           MOVE WS-RC TO WS-RC-ED
           DISPLAY " "
           DISPLAY "STERP-I-8009 SMOKE TEST COMPLETE RC="
                   FUNCTION TRIM(WS-RC-ED).
       8000-EXIT.
           EXIT.

      *>   the demo database may not carry an OPS operator - fall back
      *>   to the first active user so the smoke test still has data
       8100-ANY-USER.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 UserCode FROM core.AppUser WHERE "
                  "IsActive = 1 ORDER BY UserCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE SPACES TO R-ONE
           SET SQLIO-SINGLE TO TRUE
           CALL "SQLIO" USING SQLIO-CA R-ONE
           IF SQLIO-SQLCODE = 0
               MOVE R-ONE(1:20) TO WS-IN-USER
               PERFORM 1100-CHECK-USER THRU 1100-EXIT
           END-IF.
       8100-EXIT.
           EXIT.

       8200-DUMP-ROWS.
           IF WS-ROW-CNT = 0
               DISPLAY "(no rows)"
               GO TO 8200-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-ROW-CNT OR WS-I > WS-K
               DISPLAY FUNCTION TRIM(WS-ROW-TXT(WS-I))
           END-PERFORM
           MOVE WS-ROW-CNT TO WS-NUM-ED
           DISPLAY "ROWS AVAILABLE: " FUNCTION TRIM(WS-NUM-ED).
       8200-EXIT.
           EXIT.

      *>================================================================
      *> 9000  COMMON PANEL HANDLING
      *>================================================================
       9200-CLEAR-DET.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 15
               MOVE SPACES TO WS-DET-LINE(WS-I)
           END-PERFORM.
       9200-EXIT.
           EXIT.

       9300-SHOW-PANEL.
           MOVE SPACES TO WS-OPT
           MOVE 0 TO WS-CRT-STATUS
           DISPLAY SCR-FRAME
           ACCEPT SCR-CMD
           MOVE FUNCTION UPPER-CASE(WS-OPT) TO WS-OPT.
       9300-EXIT.
           EXIT.

       9400-PAGE-LINES.
           PERFORM 9200-CLEAR-DET THRU 9200-EXIT
           IF WS-TOP < 1 OR WS-TOP > WS-ROW-CNT
               MOVE 1 TO WS-TOP
           END-IF
           MOVE 0 TO WS-J
           PERFORM VARYING WS-I FROM WS-TOP BY 1
                   UNTIL WS-I > WS-ROW-CNT OR WS-J >= WS-PAGE-SIZE
               ADD 1 TO WS-J
               MOVE WS-ROW-TXT(WS-I) TO WS-DET-LINE(WS-J)
           END-PERFORM
           COMPUTE WS-NUM = WS-TOP + WS-J - 1
           MOVE WS-NUM TO WS-NUM-ED
           MOVE SPACES TO WS-PAGE-TXT
           MOVE WS-TOP TO WS-SEQ-ED
           MOVE WS-ROW-CNT TO WS-RC-ED
           STRING "ROWS " FUNCTION TRIM(WS-SEQ-ED) "-"
                  FUNCTION TRIM(WS-NUM-ED) " OF "
                  FUNCTION TRIM(WS-RC-ED)
               DELIMITED BY SIZE INTO WS-PAGE-TXT.
       9400-EXIT.
           EXIT.

      *>   F3/F7/F8 when the run time passes function keys through,
      *>   X/-/+ when it does not (COB_SCREEN_EXCEPTIONS not set)
       9450-NAVIGATE.
           MOVE 0 TO WS-SEL
           MOVE "N" TO WS-LEAVE
           EVALUATE TRUE
               WHEN K-F3 OR K-ESCAPE
                   MOVE "Y" TO WS-LEAVE
               WHEN WS-OPT = "X" OR WS-OPT = "END"
                   MOVE "Y" TO WS-LEAVE
               WHEN K-F7 OR K-PAGE-UP
                   PERFORM 9460-PAGE-BACK THRU 9460-EXIT
               WHEN WS-OPT = "-" OR WS-OPT = "P"
                   PERFORM 9460-PAGE-BACK THRU 9460-EXIT
               WHEN K-F8 OR K-PAGE-DOWN
                   PERFORM 9470-PAGE-FWD THRU 9470-EXIT
               WHEN WS-OPT = "+" OR WS-OPT = "N"
                   PERFORM 9470-PAGE-FWD THRU 9470-EXIT
               WHEN WS-OPT = SPACES
                   CONTINUE
               WHEN FUNCTION TEST-NUMVAL(WS-OPT) = 0
                   COMPUTE WS-SEL = FUNCTION NUMVAL(WS-OPT)
                   IF WS-SEL < 1 OR WS-SEL > WS-ROW-CNT
                       MOVE 0 TO WS-SEL
                       MOVE "STERP-W-9001 NO SUCH LINE NUMBER"
                           TO WS-MSG
                   END-IF
               WHEN OTHER
                   MOVE "STERP-W-9002 INVALID COMMAND" TO WS-MSG
           END-EVALUATE.
       9450-EXIT.
           EXIT.

       9460-PAGE-BACK.
           SUBTRACT WS-PAGE-SIZE FROM WS-TOP
           IF WS-TOP < 1
               MOVE 1 TO WS-TOP
           END-IF.
       9460-EXIT.
           EXIT.

       9470-PAGE-FWD.
           IF WS-TOP + WS-PAGE-SIZE <= WS-ROW-CNT
               ADD WS-PAGE-SIZE TO WS-TOP
           END-IF.
       9470-EXIT.
           EXIT.

       9600-SQL-ERROR.
           MOVE SPACES TO WS-MSG
           STRING "STERP-E-9009 SQL " SQLIO-SQLSTATE " "
                  SQLIO-ERRMSG(1:50)
               DELIMITED BY SIZE INTO WS-MSG
           IF SMOKE-RUN
               DISPLAY FUNCTION TRIM(WS-MSG)
               MOVE 8 TO WS-RC
           END-IF.
       9600-EXIT.
           EXIT.
       END PROGRAM STERPOPS.
      