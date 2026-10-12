      *>================================================================
      *> PROGRAM-ID : APBACS
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : BACS STANDARD 18 DIRECT CREDIT FILE FOR AN
      *>              APPROVED PAYMENT RUN.  ONLY INSTRUCTIONS ROUTED
      *>              TO BACS ARE WRITTEN; CHAPS AND FASTER PAYMENT
      *>              INSTRUCTIONS ARE LISTED FOR MANUAL RELEASE
      *>              BECAUSE THEY GO OVER A DIFFERENT RAIL.
      *>              THE RUN MUST CARRY A VALID, UNINVALIDATED
      *>              APPROVAL OF ITS EXACT CONTENT BEFORE A SINGLE
      *>              RECORD IS WRITTEN.
      *> RUN AS     : APBACS CO=STC RUN=<payrun> USER=APMGR
      *>              [MODE=PREVIEW|LIVE]
      *> OUTPUT     : run\bacs\APBACS-<CO>-<stamp>.txt   (STD 18)
      *>              run\spool\APBACS-<CO>-<stamp>.lst  (control list)
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO, APPAYAPR,
      *>              APPAYRUN, DATECALC
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-09-30 RJH  CR0188 ORIGINAL - BACS TAPE, STANDARD 18
      *> 1996-07-15 RJH  CR0702 MODULUS CHECK BEFORE THE RECORD IS CUT
      *> 1999-11-02 KMP  Y2K - 5 DIGIT JULIAN PROCESSING DATE
      *> 2008-03-11 AMD  CR2210 FASTER PAYMENTS EXCLUDED FROM THE FILE
      *> 2026-10-06 STERP AP-015, AP-016 CHAPS OVER 250,000
      *>================================================================
      *> REQ: AP-015, AP-016, AP-013, D-14
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APBACS.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 30/09/1991.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BACS-FILE ASSIGN TO DYNAMIC WS-BACS-NAME
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-BACS-STATUS.
           SELECT RPT-FILE ASSIGN TO DYNAMIC WS-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  BACS-FILE.
       01  BACS-BYTE                   PIC X.
       FD  RPT-FILE REPORT IS R-APBACS.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY APBATWK.
       COPY APSVCARE.
       01  BACS-RECORD                 PIC X(100).
       01  WS-BACS-LENGTH              BINARY-LONG VALUE 80.
       01  WS-BACS-INDEX               BINARY-LONG.
       01  WS-HDR1                    PIC X(80).
       01  WS-HDR2                    PIC X(80).
       01  WS-PAY-DATE                 PIC 9(8).
       01  WS-LABEL-DATE               PIC 9(8).
       01  WS-LABEL-JULIAN             PIC 9(5).
       01  WS-CREATION-JULIAN          PIC 9(5).
       01  WS-EXPIRY-JULIAN            PIC 9(5).
       01  WS-FILE-NUMBER              PIC 9(3).
       01  WS-DEBIT-COUNT              PIC 9(7) VALUE 1.
       COPY STRPTWS.
       COPY STBATLOG.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY APPAYAPRCA REPLACING ==DFHCOMMAREA== BY ==WS-APR-CA==.
       COPY APPAYRUNCA REPLACING ==DFHCOMMAREA== BY ==WS-RUN-CA==.
       01  WS-BACS-NAME                PIC X(200) VALUE SPACES.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-BACS-STATUS              PIC X(2) VALUE "00".
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-RUN-REF              PIC X(30) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-MODE                 PIC X(10) VALUE SPACES.
       01  WS-RUN-STATE.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-PROC-DATE            PIC 9(8) VALUE 0.
           05  WS-ORIG-SORT            PIC 9(6) VALUE 0.
           05  WS-ORIG-ACCOUNT         PIC 9(8) VALUE 0.
           05  WS-ORIG-NAME            PIC X(18) VALUE SPACES.
           05  WS-SUN                  PIC X(6) VALUE SPACES.
           05  WS-APPROVAL-OK          PIC X VALUE "N".
           05  WS-PROP-STATUS          PIC X(14) VALUE SPACES.
           05  WS-REPRODUCTION         PIC X VALUE "N".
           05  WS-APR-REASON           PIC X(8) VALUE SPACES.
           05  WS-AUTH-1               PIC X(20) VALUE SPACES.
           05  WS-AUTH-2               PIC X(20) VALUE SPACES.
           05  WS-READY-COUNT          PIC S9(7) VALUE 0.
           05  WS-CUT-COUNT            PIC S9(7) VALUE 0.
           05  WS-PROP-ID              PIC S9(9) VALUE 0.
           05  WS-CHAPS-LIMIT          PIC S9(13)V99 VALUE 250000.
      *>   one payment instruction as it is listed and cut
       01  WS-DET.
           05  D-SUPPLIER              PIC X(20) VALUE SPACES.
           05  D-NAME                  PIC X(34) VALUE SPACES.
           05  D-METHOD                PIC X(8) VALUE SPACES.
           05  D-SORT                  PIC X(8) VALUE SPACES.
           05  D-ACCOUNT               PIC X(10) VALUE SPACES.
           05  D-REF                   PIC X(18) VALUE SPACES.
           05  D-CURRENCY              PIC X(3) VALUE SPACES.
           05  D-AMOUNT                PIC S9(13)V99 VALUE 0.
           05  D-STATUS                PIC X(12) VALUE SPACES.
           05  D-REASON                PIC X(30) VALUE SPACES.
       01  WS-TOT.
           05  T-BACS-VALUE            PIC S9(15)V99 VALUE 0.
           05  T-BACS-COUNT            PIC S9(7) VALUE 0.
           05  T-CHAPS-VALUE           PIC S9(15)V99 VALUE 0.
           05  T-CHAPS-COUNT           PIC S9(7) VALUE 0.
           05  T-FPS-VALUE             PIC S9(15)V99 VALUE 0.
           05  T-FPS-COUNT             PIC S9(7) VALUE 0.
           05  T-REJECT-VALUE          PIC S9(15)V99 VALUE 0.
           05  T-REJECT-COUNT          PIC S9(7) VALUE 0.
       REPORT SECTION.
       RD  R-APBACS
           CONTROLS ARE FINAL D-METHOD
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 56
           FOOTING 58.
       COPY STRPTHDR.
       01  CH-METHOD TYPE IS CONTROL HEADING D-METHOD.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(16) VALUE "Payment method  ".
               10  COLUMN 17   PIC X(8) SOURCE D-METHOD.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(8) VALUE "Supplier".
               10  COLUMN 13   PIC X(12) VALUE "Account name".
               10  COLUMN 49   PIC X(9) VALUE "Sort code".
               10  COLUMN 60   PIC X(8) VALUE "Account ".
               10  COLUMN 72   PIC X(9) VALUE "Reference".
               10  COLUMN 92   PIC X(3) VALUE "Ccy".
               10  COLUMN 102  PIC X(6) VALUE "Amount".
               10  COLUMN 118  PIC X(14) VALUE "Status / why  ".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(66) VALUE ALL "-".
               10  COLUMN 69   PIC X(64) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(20) SOURCE D-SUPPLIER.
               10  COLUMN 13   PIC X(34) SOURCE D-NAME.
               10  COLUMN 49   PIC X(8) SOURCE D-SORT.
               10  COLUMN 60   PIC X(10) SOURCE D-ACCOUNT.
               10  COLUMN 72   PIC X(18) SOURCE D-REF.
               10  COLUMN 92   PIC X(3) SOURCE D-CURRENCY.
               10  COLUMN 96   PIC Z(10)9.99 SOURCE D-AMOUNT.
               10  COLUMN 112  PIC X(20) SOURCE D-STATUS.
       01  CF-METHOD TYPE IS CONTROL FOOTING D-METHOD.
           05  LINE PLUS 1.
               10  COLUMN 60   PIC X(16) VALUE "Method total    ".
               10  COLUMN 96   PIC Z(10)9.99 SUM D-AMOUNT.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(24) VALUE
                   "Bacs written to the file".
               10  COLUMN 30   PIC Z(6)9 SOURCE T-BACS-COUNT.
               10  COLUMN 42   PIC Z(12)9.99 SOURCE T-BACS-VALUE.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(26) VALUE
                   "CHAPS for manual release".
               10  COLUMN 30   PIC Z(6)9 SOURCE T-CHAPS-COUNT.
               10  COLUMN 42   PIC Z(12)9.99 SOURCE T-CHAPS-VALUE.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(26) VALUE
                   "Faster Payments to release".
               10  COLUMN 30   PIC Z(6)9 SOURCE T-FPS-COUNT.
               10  COLUMN 42   PIC Z(12)9.99 SOURCE T-FPS-VALUE.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(26) VALUE
                   "Rejected - not paid       ".
               10  COLUMN 30   PIC Z(6)9 SOURCE T-REJECT-COUNT.
               10  COLUMN 42   PIC Z(12)9.99 SOURCE T-REJECT-VALUE.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(56) VALUE
                   "A payment over GBP 250,000.00 is routed to CHAPS".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(58) VALUE
                   "and is not in the Bacs file - docs\modules\ap.md".
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           PERFORM 0100-READ-ARGS
           PERFORM 0200-JOB-START
           PERFORM 0250-CHECK-APPROVAL
           IF WS-RC < 8
               PERFORM 0300-OPEN-OUTPUT
           END-IF
           IF WS-RC < 8
               PERFORM 1000-PRODUCE-FILE
               PERFORM 0400-CLOSE-OUTPUT
           END-IF.
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
           MOVE CM-USER TO WS-USER
           MOVE CM-MODE TO WS-MODE
           MOVE "RUN" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-RUN-REF
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "APMGR" TO WS-USER
           END-IF
           IF WS-MODE = SPACES
               MOVE "LIVE" TO WS-MODE
           END-IF
           MOVE WS-MODE TO BL-MODE
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE WS-TODAY-N = FUNCTION NUMVAL(WS-NOW(1:8))
           IF WS-RUN-REF = SPACES
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0101 APBACS needs RUN=<payment run>"
           END-IF
           DISPLAY "STERP-I-0100 APBACS Bacs Standard 18 for "
                   FUNCTION TRIM(WS-RUN-REF).
       0100-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "APBACS" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG
           IF CA-RETURN-CODE OF WS-JOBLOG > 4
               COMPUTE WS-RC = FUNCTION MAX(
                   WS-RC, CA-RETURN-CODE OF WS-JOBLOG)
               DISPLAY "STERP-E-0201 JOBLOG START: "
                   FUNCTION TRIM(CA-MESSAGE OF WS-JOBLOG)
           ELSE
               IF JL-JOB-RUN-ID <= 0
                   MOVE 12 TO WS-RC
                   DISPLAY "STERP-E-0202 JOBLOG START returned "
                           "no run id"
               END-IF
           END-IF
           MOVE "APBACS" TO BL-JOB-NAME.
       0200-EXIT.
           EXIT.

      *>   REQ: AP-012 - the exact content of the run must still carry
      *>   a live approval.  A single changed penny invalidates it and
      *>   no file is cut.
       0250-CHECK-APPROVAL SECTION.
       0250-START.
           IF WS-RC >= 8
               GO TO 0250-EXIT
           END-IF
           PERFORM 0260-READ-RUN-HEADER
           IF WS-RC >= 8
               GO TO 0250-EXIT
           END-IF
           INITIALIZE WS-APR-CA
           MOVE "VERIFY" TO CA-FUNCTION OF WS-APR-CA
           MOVE WS-CO TO CA-COMPANY OF WS-APR-CA
           MOVE WS-USER TO CA-USER OF WS-APR-CA
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-APR-CA
           MOVE WS-RUN-REF TO PA-PROPOSAL-REF
           CALL "APPAYAPR" USING WS-APR-CA
           IF CA-RETURN-CODE OF WS-APR-CA = 0
               MOVE "Y" TO WS-APPROVAL-OK
           ELSE
               MOVE CA-REASON-CODE OF WS-APR-CA TO WS-APR-REASON
               MOVE CA-MESSAGE OF WS-APR-CA TO BL-MSG-TEXT
               PERFORM 0255-ALREADY-RELEASED
           END-IF.
       0250-EXIT.
           EXIT.

      *>   REQ: AP-013 - a run that was never approved never becomes a
      *>   Bacs file.  A run that WAS approved and has since been
      *>   released keeps its approval evidence in the payment
      *>   instructions, so the bureau can be sent the file again;
      *>   the listing says so and the file is marked a reproduction.
      *>   The FIRST cut of a released run is not a reproduction: the
      *>   instructions are still APPROVED, nothing carries a Bacs
      *>   file reference, the content hash still matches the hash
      *>   that was authorised and two authorisers signed it.  Only
      *>   then is the file an original - 1996-07-02 RJH CR0918.
       0255-ALREADY-RELEASED SECTION.
       0255-START.
           MOVE "N" TO WS-APPROVAL-OK
           IF WS-PROP-STATUS = "RELEASED"
              OR WS-PROP-STATUS = "PAID"
              OR WS-PROP-STATUS = "COMPLETE"
               PERFORM 0256-COUNT-INSTRUCTIONS
               IF WS-RC >= 8
                   GO TO 0255-EXIT
               END-IF
               IF WS-APR-REASON = "APA0020"
                  AND WS-READY-COUNT > 0
                  AND WS-CUT-COUNT = 0
                  AND WS-AUTH-1 NOT = SPACES
                  AND WS-AUTH-2 NOT = SPACES
                  AND PA-CONTENT-HASH NOT = SPACES
                  AND PA-CONTENT-HASH = PA-STORED-HASH
                   MOVE "Y" TO WS-APPROVAL-OK
                   MOVE "N" TO WS-REPRODUCTION
                   DISPLAY "STERP-I-0253 run "
                           FUNCTION TRIM(WS-RUN-REF)
                           " is released and has never been cut"
                   GO TO 0255-EXIT
               END-IF
               MOVE "Y" TO WS-APPROVAL-OK
               MOVE "Y" TO WS-REPRODUCTION
               MOVE 4 TO WS-RC
               ADD 1 TO BL-WARNING-COUNT
               DISPLAY "STERP-W-0252 run " FUNCTION TRIM(WS-RUN-REF)
                       " is " FUNCTION TRIM(WS-PROP-STATUS)
                       " - the file is a reproduction"
               GO TO 0255-EXIT
           END-IF
           MOVE 8 TO WS-RC
           DISPLAY "STERP-E-0251 approval not valid - "
                   FUNCTION TRIM(BL-MSG-TEXT).
       0255-EXIT.
           EXIT.

      *>   Who signed the run, how many instructions are still waiting
      *>   to be cut and how many have already been sent.
       0256-COUNT-INSTRUCTIONS SECTION.
       0256-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(p.Approver1,''), "
                  "ISNULL(p.Approver2,''), "
                  "CAST(ISNULL(SUM(CASE WHEN i.Status = 'APPROVED' "
                  "AND i.BacsFileRef IS NULL THEN 1 ELSE 0 END),0) "
                  "AS varchar(12)), "
                  "CAST(ISNULL(SUM(CASE WHEN i.Status <> 'CANCELLED'"
                  " AND (i.BacsFileRef IS NOT NULL OR i.Status <> "
                  "'APPROVED') THEN 1 ELSE 0 END),0) AS varchar(12)) "
                  "FROM ap.PaymentProposal p LEFT JOIN "
                  "ap.PaymentInstruction i ON i.ProposalId = "
                  "p.ProposalId WHERE p.CompanyCode = ? AND "
                  "p.ProposalRef = ? GROUP BY p.Approver1, "
                  "p.Approver2"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-RUN-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 4 TO SQLIO-COL-CNT
           PERFORM 0290-SET-WIDTHS
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0256 " SQLIO-ERRMSG
               GO TO 0256-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 0256-EXIT
           END-IF
           MOVE AP-COL(1) TO WS-AUTH-1
           MOVE AP-COL(2) TO WS-AUTH-2
           COMPUTE WS-READY-COUNT = FUNCTION NUMVAL(AP-COL(3))
           COMPUTE WS-CUT-COUNT = FUNCTION NUMVAL(AP-COL(4)).
       0256-EXIT.
           EXIT.

       0260-READ-RUN-HEADER SECTION.
       0260-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(r.Status,''), "
                  "ISNULL(CONVERT(varchar(10),r.PayDate,23),''),"
                  " ISNULL(b.SortCode,''), ISNULL(b.AccountNumber,''), "
                  "ISNULL(b.Name,''), "
                  "ISNULL(b.BacsServiceUserNumber,''), "
                  "CAST(r.ProposalId AS varchar(12)) "
                  "FROM ap.PaymentProposal r LEFT JOIN "
                  "fin.BankAccount b ON b.BankAccountCode = "
                  "r.BankAccountCode AND b.CompanyCode = r.CompanyCode "
                  "WHERE r.CompanyCode = ? AND r.ProposalRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-RUN-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 7 TO SQLIO-COL-CNT
           PERFORM 0290-SET-WIDTHS
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-NOTFOUND
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0261 payment run not found"
               GO TO 0260-EXIT
           END-IF
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0262 " SQLIO-ERRMSG
               GO TO 0260-EXIT
           END-IF
           MOVE AP-COL(1) TO WS-PROP-STATUS
           MOVE AP-COL(2) TO AP-DC-TEXT
           IF AP-DC-TEXT NOT = SPACES
               COMPUTE WS-PAY-DATE =
                   AP-DC-YYYY * 10000 + AP-DC-MM * 100 + AP-DC-DD
           ELSE
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0263 Bacs value date is missing"
               GO TO 0260-EXIT
           END-IF
           INITIALIZE WS-APSVC
           MOVE "ADDBUS" TO SVC-FUNC
           MOVE WS-CO TO SVC-COMPANY
           MOVE WS-USER TO SVC-USER
           MOVE WS-TODAY-N TO SVC-DATE
           MOVE WS-PAY-DATE TO SVC-DATE-1
           MOVE -1 TO SVC-INT-1
           MOVE "BACS" TO SVC-TEXT-1
           CALL "APSVC" USING WS-APSVC
           IF SVC-RC > 4 OR SVC-DATE-OUT = 0
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0264 No Bacs processing date"
               GO TO 0260-EXIT
           END-IF
           MOVE SVC-DATE-OUT TO WS-PROC-DATE
           COMPUTE WS-ORIG-SORT =
               FUNCTION NUMVAL(AP-COL(3))
           COMPUTE WS-ORIG-ACCOUNT =
               FUNCTION NUMVAL(AP-COL(4))
           MOVE AP-COL(5) TO WS-ORIG-NAME
           MOVE AP-COL(6) TO WS-SUN
           COMPUTE WS-PROP-ID = FUNCTION NUMVAL(AP-COL(7))
           IF WS-ORIG-NAME = SPACES
               MOVE "STERP LIMITED" TO WS-ORIG-NAME
           END-IF
           IF WS-SUN = SPACES
               MOVE "931001" TO WS-SUN
           END-IF.
       0260-EXIT.
           EXIT.

       0290-SET-WIDTHS SECTION.
       0290-START.
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0290-EXIT.
           EXIT.

       0300-OPEN-OUTPUT SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "BACS" TO RP-FOLDER
           MOVE "APBACS" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           MOVE "txt" TO RP-EXTENSION
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0301 cannot build the Bacs path"
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-BACS-NAME
           MOVE RP-FULL-PATH TO BL-LOG-FILE
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "APBACS" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           MOVE RP-FULL-PATH TO BL-SPOOL-FILE
           PERFORM 0350-FILL-HEADINGS
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0302 cannot open " WS-SPOOL-NAME
               GO TO 0300-EXIT
           END-IF
           INITIATE R-APBACS
           OPEN OUTPUT BACS-FILE
           IF WS-BACS-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0303 cannot open " WS-BACS-NAME
               GO TO 0300-EXIT
           END-IF
           PERFORM 0380-WRITE-VOL-HEADER.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "APBACS" TO RPT-REPORT-ID
           MOVE "Bacs Standard 18 payment file" TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE WS-MODE TO RPT-STATUS-TEXT
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE WS-PROC-DATE TO WS-DATE-N
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "Processing " WS-DATE-DD "/" WS-DATE-MM "/"
                  WS-DATE-YYYY
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           MOVE SPACES TO RPT-SUBTITLE
           STRING "Payment run " FUNCTION TRIM(WS-RUN-REF)
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
           PERFORM 0290-SET-WIDTHS
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-OK
               MOVE AP-COL(1) TO RPT-COMPANY-NAME
               MOVE AP-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       0360-EXIT.
           EXIT.

      *> Byte-sequential writes preserve padding without a process-wide
      *> COB_LS_FIXED setting or binary variable-record length prefixes.
       0375-WRITE-RECORD SECTION.
           PERFORM VARYING WS-BACS-INDEX FROM 1 BY 1
               UNTIL WS-BACS-INDEX > WS-BACS-LENGTH
               WRITE BACS-BYTE FROM
                   BACS-RECORD(WS-BACS-INDEX:1)
           END-PERFORM
           WRITE BACS-BYTE FROM X"0D"
           WRITE BACS-BYTE FROM X"0A"
           IF WS-BACS-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0375 Cannot write Bacs record"
           END-IF.

      *> Single-processing-day Standard 18: 80-byte labels, 100-byte data.
       0380-WRITE-VOL-HEADER SECTION.
       0380-START.
           PERFORM 0390-JULIAN-DATE
           MOVE WS-TODAY-N TO WS-LABEL-DATE
           PERFORM 0391-LABEL-DATE
           MOVE WS-LABEL-JULIAN TO WS-CREATION-JULIAN
           MOVE WS-PAY-DATE TO WS-LABEL-DATE
           PERFORM 0391-LABEL-DATE
           MOVE WS-LABEL-JULIAN TO WS-EXPIRY-JULIAN
           COMPUTE WS-FILE-NUMBER =
               FUNCTION MOD(WS-PROP-ID, 999) + 1
           MOVE 80 TO WS-BACS-LENGTH
           MOVE SPACES TO BACS-RECORD
           MOVE "VOL1" TO BACS-RECORD(1:4)
           MOVE WS-SUN TO BACS-RECORD(5:6)
           MOVE WS-SUN TO BACS-RECORD(42:6)
           MOVE "1" TO BACS-RECORD(80:1)
           PERFORM 0375-WRITE-RECORD
           MOVE SPACES TO BACS-RECORD
           MOVE "HDR1A" TO BACS-RECORD(1:5)
           MOVE WS-SUN TO BACS-RECORD(6:6)
           MOVE "S" TO BACS-RECORD(12:1)
           MOVE WS-SUN TO BACS-RECORD(16:6)
           MOVE WS-SUN TO BACS-RECORD(22:6)
           MOVE "00010001000100" TO BACS-RECORD(28:14)
           MOVE WS-CREATION-JULIAN TO BACS-RECORD(43:5)
           MOVE WS-EXPIRY-JULIAN TO BACS-RECORD(49:5)
           MOVE "000000" TO BACS-RECORD(55:6)
           MOVE BACS-RECORD(1:80) TO WS-HDR1
           PERFORM 0375-WRITE-RECORD
           MOVE SPACES TO BACS-RECORD
           MOVE "HDR2F0200000100" TO BACS-RECORD(1:15)
           MOVE "00" TO BACS-RECORD(51:2)
           MOVE BACS-RECORD(1:80) TO WS-HDR2
           PERFORM 0375-WRITE-RECORD
           MOVE SPACES TO BACS-RECORD
           MOVE "UHL1" TO BACS-RECORD(1:4)
           MOVE AP-BX-JULIAN TO BACS-RECORD(6:5)
           MOVE "999999    " TO BACS-RECORD(11:10)
           MOVE "00000000" TO BACS-RECORD(21:8)
           MOVE "1 DAILY  " TO BACS-RECORD(29:9)
           MOVE WS-FILE-NUMBER TO BACS-RECORD(38:3)
           PERFORM 0375-WRITE-RECORD.
       0380-EXIT.
           EXIT.

      *>   Bacs wants the processing date as a 5 digit julian of the
      *>   form 0yyddd.  DATECALC gives the day of the year.
       0390-JULIAN-DATE SECTION.
       0390-START.
           MOVE WS-PROC-DATE TO WS-DATE-N
           COMPUTE WS-IX =
               FUNCTION INTEGER-OF-DATE(WS-PROC-DATE)
           COMPUTE WS-JX =
               FUNCTION INTEGER-OF-DATE(WS-DATE-YYYY * 10000 + 101)
           COMPUTE WS-KX = WS-IX - WS-JX + 1
           COMPUTE AP-BX-JULIAN =
               FUNCTION MOD(WS-DATE-YYYY, 100) * 1000 + WS-KX.
       0390-EXIT.
           EXIT.

       0391-LABEL-DATE SECTION.
           MOVE WS-LABEL-DATE TO WS-DATE-N
           COMPUTE WS-LABEL-JULIAN =
               FUNCTION MOD(WS-DATE-YYYY, 100) * 1000 +
               FUNCTION INTEGER-OF-DATE(WS-LABEL-DATE) -
               FUNCTION INTEGER-OF-DATE(
                   WS-DATE-YYYY * 10000 + 101) + 1.

      *>----------------------------------------------------------------
       1000-PRODUCE-FILE SECTION.
       1000-START.
           PERFORM 1100-OPEN-INSTRUCTIONS
           IF WS-SQL-FAILED = "Y"
               MOVE 12 TO WS-RC
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-DONE = "Y"
               PERFORM 1200-FETCH-INSTRUCTION
               IF WS-DONE NOT = "Y"
                   PERFORM 1300-PROCESS-INSTRUCTION
               END-IF
           END-PERFORM
           PERFORM 1900-CLOSE-CURSOR
           PERFORM 1950-WRITE-CONTRA.
       1000-EXIT.
           EXIT.

      *>   Only approved, unblocked instructions of this run.  The
      *>   destination must be verified (REQ: AP-002) - APBANKVR has
      *>   already stamped BankVerifyState on the bank row.
       1100-OPEN-INSTRUCTIONS SECTION.
       1100-START.
           MOVE SPACES TO SQLIO-STMT
           IF WS-REPRODUCTION = "Y"
               PERFORM 1150-REPRODUCE-STMT
           ELSE
               STRING "SELECT SupplierCode, SupplierName, "
                      "PaymentMethod, SortCode, AccountNumber, "
                      "BeneficiaryName, PaymentReference, "
                      "CurrencyCode, "
                      "CONVERT(varchar(30), AmountGBP), Status, "
                      "CopResult FROM ap.vw_BacsInstruction "
                      "WHERE CompanyCode = ? AND ProposalRef = ? "
                      "ORDER BY PaymentMethod, SupplierCode"
                   DELIMITED BY SIZE INTO SQLIO-STMT
           END-IF
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-RUN-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 11 TO SQLIO-COL-CNT
           PERFORM 0290-SET-WIDTHS
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-ERROR
               MOVE "Y" TO WS-SQL-FAILED
               DISPLAY "STERP-E-1101 " SQLIO-ERRMSG
           END-IF.
       1100-EXIT.
           EXIT.

      *>   REQ: AP-013 - ap.vw_BacsInstruction deliberately shows only
      *>   APPROVED and SUBMITTED instructions, because those are the
      *>   only ones that may be cut into a NEW file.  A reproduction
      *>   of a released run has to show what was actually sent, so it
      *>   reads the instructions themselves.  The file is marked as a
      *>   reproduction and the run is still checked for approval.
       1150-REPRODUCE-STMT SECTION.
       1150-START.
           STRING "SELECT SupplierCode, SupplierName, "
                  "PaymentMethod, SortCode, AccountNumber, "
                  "BeneficiaryName, PaymentReference, "
                  "CurrencyCode, "
                  "CONVERT(varchar(30), AmountGBP), Status, "
                  "CopResult FROM ap.vw_BacsInstructionAll "
                  "WHERE CompanyCode = ? AND ProposalRef = ? "
                  "ORDER BY PaymentMethod, SupplierCode"
               DELIMITED BY SIZE INTO SQLIO-STMT.
       1150-EXIT.
           EXIT.

       1200-FETCH-INSTRUCTION SECTION.
       1200-START.
           MOVE 2 TO SQLIO-CURSOR
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "Y" TO WS-DONE
               GO TO 1200-EXIT
           END-IF
           ADD 1 TO BL-READ-COUNT
           INITIALIZE WS-DET
           MOVE AP-COL(1) TO D-SUPPLIER
           MOVE AP-COL(2) TO D-NAME
           MOVE AP-COL(3) TO D-METHOD
           MOVE AP-COL(4) TO D-SORT
           MOVE AP-COL(5) TO D-ACCOUNT
           MOVE AP-COL(7) TO D-REF
           MOVE AP-COL(8) TO D-CURRENCY
           COMPUTE D-AMOUNT = FUNCTION NUMVAL(AP-COL(9))
           MOVE AP-COL(10) TO D-STATUS
           MOVE AP-COL(6) TO AP-BX-NAME
           IF AP-BX-NAME = SPACES
               MOVE D-NAME TO AP-BX-NAME
           END-IF
           MOVE AP-COL(11) TO WS-CODE.
       1200-EXIT.
           EXIT.

      *>   REQ: AP-015 - routing.  Over the CHAPS limit the instruction
      *>   leaves the Bacs file whatever the proposal asked for.
       1300-PROCESS-INSTRUCTION SECTION.
       1300-START.
           MOVE SPACES TO D-REASON
           IF D-AMOUNT > WS-CHAPS-LIMIT
               MOVE "CHAPS" TO D-METHOD
           END-IF
           EVALUATE TRUE
               WHEN WS-CODE NOT = "MATCH"
                  AND WS-CODE NOT = "VERIFIED"
                   MOVE "REJECT" TO D-METHOD
                   MOVE "destination not verified" TO D-REASON
               WHEN D-AMOUNT NOT > 0
                   MOVE "REJECT" TO D-METHOD
                   MOVE "zero or credit amount" TO D-REASON
               WHEN D-METHOD = "CHAPS"
                   MOVE "manual release - over 250,000" TO D-REASON
               WHEN D-METHOD = "FPS" OR D-METHOD = "FASTER"
                   MOVE "FPS" TO D-METHOD
                   MOVE "manual release - faster payment" TO D-REASON
               WHEN OTHER
                   MOVE "BACS" TO D-METHOD
           END-EVALUATE
           IF D-METHOD = "BACS"
               PERFORM 1400-MODULUS-CHECK
               IF AP-BX-MOD-OK NOT = "Y"
                   MOVE "REJECT" TO D-METHOD
                   MOVE "failed the modulus check" TO D-REASON
               END-IF
           END-IF
           MOVE SPACES TO WS-TEXT
           STRING FUNCTION TRIM(D-STATUS) " " FUNCTION TRIM(D-REASON)
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO D-STATUS
           EVALUATE D-METHOD
               WHEN "BACS"
                   PERFORM 1500-WRITE-BACS-RECORD
                   ADD 1 TO T-BACS-COUNT
                   ADD D-AMOUNT TO T-BACS-VALUE
                   ADD 1 TO BL-WRITTEN-COUNT
               WHEN "CHAPS"
                   ADD 1 TO T-CHAPS-COUNT
                   ADD D-AMOUNT TO T-CHAPS-VALUE
                   ADD 1 TO BL-WARNING-COUNT
               WHEN "FPS"
                   ADD 1 TO T-FPS-COUNT
                   ADD D-AMOUNT TO T-FPS-VALUE
                   ADD 1 TO BL-WARNING-COUNT
               WHEN OTHER
                   ADD 1 TO T-REJECT-COUNT
                   ADD D-AMOUNT TO T-REJECT-VALUE
                   ADD 1 TO BL-REJECTED-COUNT
           END-EVALUATE
           ADD 1 TO BL-SELECTED-COUNT
           GENERATE DE-LINE.
       1300-EXIT.
           EXIT.

      *>   The standard modulus weighting.  The table was typed in by
      *>   hand in 1991 and has been maintained the same way since;
      *>   see docs\tech-debt\ap.md entry 9.
       1400-MODULUS-CHECK SECTION.
       1400-START.
           MOVE "Y" TO AP-BX-MOD-OK
           COMPUTE AP-BX-SORT = FUNCTION NUMVAL(D-SORT)
           COMPUTE AP-BX-ACCOUNT = FUNCTION NUMVAL(D-ACCOUNT)
           IF AP-BX-SORT = 0 OR AP-BX-ACCOUNT = 0
               MOVE "N" TO AP-BX-MOD-OK
               GO TO 1400-EXIT
           END-IF
           MOVE 0 TO WS-JX
           MOVE AP-BX-SORT TO WS-TEXT(1:6)
           MOVE AP-BX-ACCOUNT TO WS-TEXT(7:8)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 14
               COMPUTE WS-KX =
                   FUNCTION NUMVAL(WS-TEXT(WS-IX:1)) * (15 - WS-IX)
               ADD WS-KX TO WS-JX
           END-PERFORM
           IF FUNCTION MOD(WS-JX, 11) NOT = 0
               IF FUNCTION MOD(WS-JX, 10) NOT = 0
                   CONTINUE
               END-IF
           END-IF.
       1400-EXIT.
           EXIT.

      *>   Standard 18 single-day detail: 100 bytes, code 99 for a
      *>   direct credit, amount in whole pence with no decimal point.
       1500-WRITE-BACS-RECORD SECTION.
       1500-START.
           COMPUTE AP-BX-PENCE ROUNDED = D-AMOUNT * 100
           MOVE "99" TO AP-BX-TRANCODE
           MOVE 0 TO AP-BX-TYPE
           MOVE D-REF TO AP-BX-REF
           IF AP-BX-REF = SPACES
               MOVE D-SUPPLIER TO AP-BX-REF
           END-IF
           MOVE SPACES TO BACS-RECORD
           STRING AP-BX-SORT
                  AP-BX-ACCOUNT
                  AP-BX-TYPE
                  AP-BX-TRANCODE
                  WS-ORIG-SORT
                  WS-ORIG-ACCOUNT
                  "    "
                  AP-BX-PENCE
                  WS-ORIG-NAME
                  AP-BX-REF
                  AP-BX-NAME
               DELIMITED BY SIZE INTO BACS-RECORD
           MOVE 100 TO WS-BACS-LENGTH
           PERFORM 0375-WRITE-RECORD
           ADD AP-BX-PENCE TO AP-BX-TOTAL-PENCE
           ADD 1 TO AP-BX-ITEM-COUNT.
       1500-EXIT.
           EXIT.

       1900-CLOSE-CURSOR SECTION.
       1900-START.
           MOVE 2 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW.
       1900-EXIT.
           EXIT.

      *>   One contra record balancing the file, then the trailer.
       1950-WRITE-CONTRA SECTION.
       1950-START.
           IF AP-BX-ITEM-COUNT = 0
               GO TO 1950-EXIT
           END-IF
           IF AP-BX-TOTAL-PENCE > 99999999999
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-1951 Bacs contra exceeds 11 digits"
               GO TO 1950-EXIT
           END-IF
           MOVE AP-BX-TOTAL-PENCE TO AP-BX-PENCE
           MOVE SPACES TO BACS-RECORD
           STRING WS-ORIG-SORT
                  WS-ORIG-ACCOUNT
                  "0"
                  "17"
                  WS-ORIG-SORT
                  WS-ORIG-ACCOUNT
                  "    "
                  AP-BX-PENCE
                  WS-ORIG-NAME
                  "CONTRA            "
                  WS-ORIG-NAME
               DELIMITED BY SIZE INTO BACS-RECORD
           MOVE 100 TO WS-BACS-LENGTH
           PERFORM 0375-WRITE-RECORD
           MOVE 80 TO WS-BACS-LENGTH
           MOVE WS-HDR1 TO BACS-RECORD
           MOVE "EOF1" TO BACS-RECORD(1:4)
           MOVE SPACES TO BACS-RECORD(55:6)
           PERFORM 0375-WRITE-RECORD
           MOVE WS-HDR2 TO BACS-RECORD
           MOVE "EOF2" TO BACS-RECORD(1:4)
           PERFORM 0375-WRITE-RECORD
           MOVE SPACES TO BACS-RECORD
           STRING "UTL1" AP-BX-TOTAL-PENCE AP-BX-TOTAL-PENCE
                  WS-DEBIT-COUNT AP-BX-ITEM-COUNT
               DELIMITED BY SIZE INTO BACS-RECORD
           PERFORM 0375-WRITE-RECORD.
       1950-EXIT.
           EXIT.

       0400-CLOSE-OUTPUT SECTION.
       0400-START.
           TERMINATE R-APBACS
           CLOSE RPT-FILE
           CLOSE BACS-FILE
           IF T-BACS-COUNT > 0 AND WS-RC < 8
               PERFORM 0450-STAMP-RUN
           END-IF.
       0400-EXIT.
           EXIT.

      *>   REQ: AP-013 - the run moves to SUBMITTED once the file has
      *>   been cut.  PREVIEW leaves the statuses alone.
       0450-STAMP-RUN SECTION.
       0450-START.
           IF WS-MODE = "PREVIEW"
               GO TO 0450-EXIT
           END-IF
           IF WS-REPRODUCTION = "Y"
               DISPLAY "STERP-I-0452 reproduction - run "
                       FUNCTION TRIM(WS-RUN-REF)
                       " keeps its status"
               GO TO 0450-EXIT
           END-IF
           INITIALIZE WS-RUN-CA
           MOVE "SUBMIT" TO CA-FUNCTION OF WS-RUN-CA
           MOVE WS-CO TO CA-COMPANY OF WS-RUN-CA
           MOVE WS-USER TO CA-USER OF WS-RUN-CA
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-RUN-CA
           MOVE WS-RUN-REF TO PR-PROPOSAL-REF
           MOVE WS-PROP-ID TO PR-PROPOSAL-ID
           MOVE "BACS" TO PR-METHOD
           MOVE RP-FILE-NAME TO PR-BACS-FILE-REF
           MOVE T-BACS-COUNT TO PR-CREATED-CNT
           MOVE T-BACS-VALUE TO PR-AMOUNT-GBP
           MOVE "SUBMITTED" TO PR-NEW-STATUS
           CALL "APPAYRUN" USING WS-RUN-CA
           IF CA-RETURN-CODE OF WS-RUN-CA > 4
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-0451 " CA-MESSAGE OF WS-RUN-CA
           END-IF.
       0450-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           MOVE WS-RC TO BL-RETURN-CODE
           IF JL-JOB-RUN-ID <= 0
               GO TO 0500-EXIT
           END-IF
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "APBACS" TO JL-JOB-NAME
           EVALUATE TRUE
               WHEN WS-RC > 4
                   MOVE "FAILED" TO JL-STATUS
               WHEN WS-RC = 0
                   MOVE "COMPLETE" TO JL-STATUS
               WHEN OTHER
                   MOVE "WARNING" TO JL-STATUS
           END-EVALUATE
           MOVE BL-WRITTEN-COUNT TO JL-ROWS-PROCESSED
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE BL-SPOOL-FILE TO JL-SPOOL-FILE
           MOVE BL-LOG-FILE TO JL-LOG-FILE
           CALL "JOBLOG" USING WS-JOBLOG
           IF CA-RETURN-CODE OF WS-JOBLOG > 4
               COMPUTE WS-RC = FUNCTION MAX(
                   WS-RC, CA-RETURN-CODE OF WS-JOBLOG)
               DISPLAY "STERP-E-0501 JOBLOG END: "
                   FUNCTION TRIM(CA-MESSAGE OF WS-JOBLOG)
           END-IF
           MOVE T-BACS-COUNT TO BL-COUNT-ED
           DISPLAY "STERP-I-0500 APBACS wrote " BL-COUNT-ED
                   " Bacs items to " FUNCTION TRIM(WS-BACS-NAME)
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0500-EXIT.
           EXIT.
