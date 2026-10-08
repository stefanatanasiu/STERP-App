      *>================================================================
      *> PROGRAM-ID : ARRECON
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : SALES LEDGER CONTROL ACCOUNT RECONCILIATION AND
      *>              PERIOD CLOSE PROOF.  REBUILDS THE OPEN ITEM
      *>              DETAIL AS AT THE PERIOD END (OR A GIVEN DATE),
      *>              SPLITS IT INTO OUTSTANDING DEBT, UNAPPLIED
      *>              CREDIT NOTES, CASH ON ACCOUNT, CUSTOMER
      *>              DEPOSITS AND ALLOWANCES, AND PROVES THE TOTAL
      *>              AGAINST THE GENERAL LEDGER AR CONTROL ACCOUNTS
      *>              ON THE SAME DATE.  ANY JOURNAL POSTED STRAIGHT
      *>              TO A CONTROL ACCOUNT WITHOUT A SALES LEDGER
      *>              SUBLEDGER REFERENCE IS LISTED, BECAUSE THAT IS
      *>              ALWAYS THE REASON FOR A DIFFERENCE.  ADJUSTMENTS
      *>              AND WRITE OFFS ARE LISTED BY TYPE SO THAT
      *>              CREDITS, WRITE OFFS, TRANSFERS AND CORRECTIONS
      *>              REMAIN DISTINGUISHABLE (AR-020).  AN OUT OF
      *>              BALANCE PROOF RAISES AN OWNED EXCEPTION.
      *> RUN AS     : ARRECON CO=STC PERIOD=202607 USER=ARCLERK1
      *>              ARRECON CO=STC ASAT=20261031 USER=ARCLERK1
      *> OUTPUT     : run\spool\ARRECON-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, EXCPRAIS, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-04-16 RJH  ORIGINAL CONTROL ACCOUNT PROOF (MVS PORT)
      *> 1998-08-19 RJH  CR0912 UNAPPLIED CASH SPLIT FROM CREDIT NOTES
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS
      *> 2011-05-24 KMP  CR2388 DIRECT JOURNALS TO THE CONTROL ACCOUNT
      *>                 LISTED AFTER THE 2010 AUDIT QUALIFICATION
      *> 2026-10-05 STERP AR-021 / AR-020 PERIOD CLOSE PROOF
      *>================================================================
      *> REQ: AR-021, AR-020
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARRECON.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 16/04/1991.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO DYNAMIC WS-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-ARRECON.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ARWORK.
       COPY STRPTWS.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXCPRAIS==.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  BR-ROW.
           05  BR-COL OCCURS 20 TIMES  PIC X(120).
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-PERIOD               PIC X(6) VALUE SPACES.
           05  WS-FY                   PIC 9(4) VALUE 0.
           05  WS-PP                   PIC 9(2) VALUE 0.
           05  WS-ASAT                 PIC 9(8) VALUE 0.
           05  WS-PSTAT                PIC X(8) VALUE SPACES.
       01  WS-RUN.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-JOBID                PIC S9(9) COMP-5 VALUE 0.
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-DIRECT               PIC S9(13)V99 VALUE 0.
           05  WS-ADJ-CNT              PIC S9(7) VALUE 0.
           05  WS-GL-CNT               PIC S9(5) VALUE 0.
       01  WS-UK.
           05  WS-UK-DD                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-UK-MM                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-UK-YYYY              PIC 9(4) VALUE 0.
      *>   the proof buckets.  LEDGER is the arithmetic sum of every
      *>   posted document, which is what the control account holds.
       01  WS-BUCKETS.
           05  K-DEBT                  PIC S9(13)V99 VALUE 0.
           05  K-CREDIT                PIC S9(13)V99 VALUE 0.
           05  K-CASH                  PIC S9(13)V99 VALUE 0.
           05  K-DEPOSIT               PIC S9(13)V99 VALUE 0.
           05  K-ALLOW                 PIC S9(13)V99 VALUE 0.
           05  K-LEDGER                PIC S9(13)V99 VALUE 0.
           05  K-GL                    PIC S9(13)V99 VALUE 0.
           05  K-DIFF                  PIC S9(13)V99 VALUE 0.
           05  K-ITEMS                 PIC S9(9) VALUE 0.
       01  WS-DET.
           05  X-LABEL                 PIC X(38) VALUE SPACES.
           05  X-REF                   PIC X(20) VALUE SPACES.
           05  X-DATE                  PIC X(10) VALUE SPACES.
           05  X-NAME                  PIC X(24) VALUE SPACES.
           05  X-AMT1                  PIC S9(13)V99 VALUE 0.
           05  X-AMT2                  PIC S9(13)V99 VALUE 0.
       REPORT SECTION.
       RD  R-ARRECON
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 8 LAST DETAIL 54
           FOOTING 58.
       COPY STRPTHDR.
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(38) SOURCE X-LABEL.
               10  COLUMN 40   PIC X(20) SOURCE X-REF.
               10  COLUMN 61   PIC X(10) SOURCE X-DATE.
               10  COLUMN 72   PIC X(24) SOURCE X-NAME.
               10  COLUMN 97   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE X-AMT1.
               10  COLUMN 115  PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE X-AMT2.
       01  DE-TEXT TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(38) SOURCE X-LABEL.
               10  COLUMN 40   PIC X(20) SOURCE X-REF.
               10  COLUMN 61   PIC X(10) SOURCE X-DATE.
               10  COLUMN 72   PIC X(24) SOURCE X-NAME.
       01  DE-RULE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-GAP TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X VALUE SPACE.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(40) VALUE
                   "Open items reconstructed as at the date".
               10  COLUMN 42   PIC ZZZ,ZZZ,ZZ9 SOURCE K-ITEMS.
               10  COLUMN 60   PIC X(30) VALUE
                   "Control accounts proved".
               10  COLUMN 92   PIC ZZZZ9 SOURCE WS-GL-CNT.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(48) VALUE
                   "Balances are derived from posted documents only.".
               10  COLUMN 50   PIC X(44) VALUE
                   "A customer balance is never edited directly.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-READ-ARGS
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0200-JOB-START
           PERFORM 0150-RESOLVE-DATE
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0300-OPEN-SPOOL
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 1000-SUBLEDGER
           PERFORM 2000-CONTROL-ACCOUNTS
           PERFORM 3000-PROOF
           PERFORM 4000-ADJUSTMENTS
           PERFORM 0400-CLOSE-SPOOL
           IF K-DIFF > 0.005 OR K-DIFF < -0.005
               PERFORM 5000-RAISE-DIFFERENCE
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
           MOVE CM-PERIOD TO WS-PERIOD
           IF CM-ASAT NOT = SPACES
               COMPUTE WS-ASAT = FUNCTION NUMVAL(CM-ASAT)
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "ARCLERK1" TO WS-USER
           END-IF
           IF WS-PERIOD = SPACES AND WS-ASAT = 0
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0105 give PERIOD=yyyypp or ASAT="
               GO TO 0100-EXIT
           END-IF
           DISPLAY "STERP-I-0100 ARRECON control account proof for "
                   FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

       0150-RESOLVE-DATE SECTION.
       0150-START.
           MOVE "n/a" TO WS-PSTAT
           IF WS-PERIOD = SPACES
               GO TO 0150-EXIT
           END-IF
           COMPUTE WS-FY = FUNCTION NUMVAL(WS-PERIOD(1:4))
           COMPUTE WS-PP = FUNCTION NUMVAL(WS-PERIOD(5:2))
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(char(8),EndDate,112), Status FROM "
                  "fin.Period WHERE CompanyCode = ? AND FiscalYear "
                  "= ? AND PeriodNo = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-FY TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE WS-PP TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 8000-SET-WIDTHS
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF NOT SQLIO-OK
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0151 period not found"
               GO TO 0150-EXIT
           END-IF
           MOVE BR-COL(1) TO AR-COMPACT-DATE
           COMPUTE WS-ASAT = FUNCTION NUMVAL(AR-COMPACT-DATE)
           MOVE BR-COL(2) TO WS-PSTAT.
       0150-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "ARRECON" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE JL-JOB-RUN-ID TO WS-JOBID.
       0200-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "ARRECON" TO RP-REPORT-ID
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
           INITIATE R-ARRECON.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "ARRECON" TO RPT-REPORT-ID
           MOVE "Sales ledger control account reconciliation"
               TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE FUNCTION CURRENT-DATE TO AR-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(AR-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(AR-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(AR-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(AR-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(AR-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(AR-NOW(13:2))
           MOVE WS-ASAT TO AR-NUM-DATE
           PERFORM 8100-FORMAT-DATE
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "As at " WS-UK DELIMITED BY SIZE
               INTO RPT-AS-AT-TEXT
           MOVE SPACES TO RPT-SUBTITLE
           IF WS-PERIOD = SPACES
               STRING "Date basis - amounts in GBP"
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           ELSE
               STRING "Period " FUNCTION TRIM(WS-PERIOD)
                      " (" FUNCTION TRIM(WS-PSTAT)
                      ") - amounts in GBP"
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
           PERFORM 8000-SET-WIDTHS
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-OK
               MOVE BR-COL(1) TO RPT-COMPANY-NAME
               MOVE BR-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       0360-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-021  open item detail, credits, deposits, allowances
      *>----------------------------------------------------------------
       1000-SUBLEDGER SECTION.
       1000-START.
           MOVE "Sales ledger detail by item type" TO X-LABEL
           MOVE "Posted to date" TO X-REF
           MOVE "Outstanding" TO X-NAME
           PERFORM 8700-PUT-TEXT
           PERFORM 8750-PUT-RULE
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT t.ItemType, CONVERT(varchar(24),SUM("
                  "t.Orig)), CONVERT(varchar(24),SUM(t.Resid)), "
                  "CONVERT(varchar(12),COUNT(*)) FROM (SELECT "
                  "o.ItemType, o.OriginalAmountGBP AS Orig, "
                  "o.OriginalAmountGBP - ISNULL((SELECT SUM("
                  "a.AppliedAmountGBP + a.DiscountAmountGBP + "
                  "a.WriteOffAmountGBP) FROM ar.Allocation a WHERE "
                  "a.TargetOpenItemId = o.OpenItemId AND a.Status = "
                  "'ACTIVE' AND a.EffectiveDate <= ?),0) + ISNULL(("
                  "SELECT SUM(b.AppliedAmountGBP) FROM ar.Allocation "
                  "b WHERE b.SourceOpenItemId = o.OpenItemId AND "
                  "b.Status = 'ACTIVE' AND b.EffectiveDate <= ?),0) "
                  "AS Resid FROM ar.OpenItem o WHERE o.CompanyCode "
                  "= ? AND o.PostingDate <= ? AND o.SettlementStatus"
                  " <> 'REVERSED') t GROUP BY t.ItemType "
                  "ORDER BY t.ItemType"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-ASAT TO AR-DATE-ED
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(1) SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE WS-CO TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE 4 TO SQLIO-COL-CNT
           PERFORM 8000-SET-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1001 " SQLIO-ERRMSG(1:100)
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO AR-DONE
           PERFORM UNTIL AR-FINISHED
               MOVE 1 TO SQLIO-CURSOR
               MOVE 4 TO SQLIO-COL-CNT
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO AR-DONE
               ELSE
                   PERFORM 1100-ONE-TYPE
               END-IF
           END-PERFORM
           MOVE 1 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           PERFORM 8750-PUT-RULE.
       1000-EXIT.
           EXIT.

       1100-ONE-TYPE SECTION.
       1100-START.
           MOVE SPACES TO X-LABEL X-REF X-DATE X-NAME
           MOVE BR-COL(1) TO X-LABEL
           COMPUTE X-AMT1 = FUNCTION NUMVAL(BR-COL(2))
           COMPUTE X-AMT2 = FUNCTION NUMVAL(BR-COL(3))
           COMPUTE AR-N = FUNCTION NUMVAL(BR-COL(4))
           MOVE AR-N TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO X-REF
           ADD AR-N TO K-ITEMS
      *>       CR2611  the sales ledger BALANCE is what is still
      *>       outstanding, not the sum of the documents as raised: a
      *>       settlement discount or a write off on an allocation
      *>       leaves the customer owing less and is posted to the
      *>       control account.  AGEDDEBT and RECONCTL prove the same
      *>       figure, so the three can no longer disagree.
           ADD X-AMT2 TO K-LEDGER
           EVALUATE BR-COL(1)(1:10)
               WHEN "INVOICE   "
                   ADD X-AMT2 TO K-DEBT
               WHEN "INTEREST  "
                   ADD X-AMT2 TO K-DEBT
               WHEN "CREDIT    "
                   ADD X-AMT2 TO K-CREDIT
               WHEN "RECEIPT   "
                   ADD X-AMT2 TO K-CASH
               WHEN "DEPOSIT   "
                   ADD X-AMT2 TO K-DEPOSIT
               WHEN OTHER
      *>           adjustments and write offs are the allowances
                   ADD X-AMT2 TO K-ALLOW
           END-EVALUATE
           ADD 1 TO WS-ROWS-OUT
           GENERATE DE-LINE.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-021  the general ledger side of the proof
      *>----------------------------------------------------------------
       2000-CONTROL-ACCOUNTS SECTION.
       2000-START.
           PERFORM 8800-PUT-GAP
           MOVE "General ledger AR control accounts" TO X-LABEL
           MOVE "Account" TO X-REF
           MOVE "Balance / direct" TO X-NAME
           PERFORM 8700-PUT-TEXT
           PERFORM 8750-PUT-RULE
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ac.AccountCode, ac.Name, "
                  "CONVERT(varchar(24),ISNULL(SUM("
                  "l.DebitAmountGBP - l.CreditAmountGBP),0)), "
                  "CONVERT(varchar(24),ISNULL(SUM(CASE WHEN "
                  "ISNULL(l.SubledgerType,'') <> 'AR' THEN "
                  "l.DebitAmountGBP - l.CreditAmountGBP ELSE 0 "
                  "END),0)) FROM fin.Account ac LEFT JOIN "
                  "fin.JournalLine l ON l.CompanyCode = "
                  "ac.CompanyCode AND l.AccountCode = ac.AccountCode "
                  "LEFT JOIN fin.Journal j ON j.JournalId = "
                  "l.JournalId AND j.Status = 'POSTED' AND "
                  "j.PostingDate <= ? WHERE ac.CompanyCode = ? AND "
                  "ac.ControlType = 'AR' GROUP BY ac.AccountCode, "
                  "ac.Name ORDER BY ac.AccountCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-ASAT TO AR-DATE-ED
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-CO TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 4 TO SQLIO-COL-CNT
           PERFORM 8000-SET-WIDTHS
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-2001 " SQLIO-ERRMSG(1:100)
               GO TO 2000-EXIT
           END-IF
           MOVE "N" TO AR-DONE
           PERFORM UNTIL AR-FINISHED
               MOVE 2 TO SQLIO-CURSOR
               MOVE 4 TO SQLIO-COL-CNT
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO AR-DONE
               ELSE
                   PERFORM 2100-ONE-ACCOUNT
               END-IF
           END-PERFORM
           MOVE 2 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           PERFORM 8750-PUT-RULE
           IF WS-GL-CNT = 0
               MOVE 4 TO WS-RC
               MOVE "No AR control account is designated" TO X-LABEL
               MOVE SPACES TO X-REF X-DATE X-NAME
               PERFORM 8700-PUT-TEXT
           END-IF.
       2000-EXIT.
           EXIT.

       2100-ONE-ACCOUNT SECTION.
       2100-START.
           ADD 1 TO WS-GL-CNT
           MOVE SPACES TO X-LABEL X-REF X-DATE X-NAME
           MOVE BR-COL(2) TO X-LABEL
           MOVE BR-COL(1) TO X-REF
           COMPUTE X-AMT1 = FUNCTION NUMVAL(BR-COL(3))
           COMPUTE X-AMT2 = FUNCTION NUMVAL(BR-COL(4))
           ADD X-AMT1 TO K-GL
           ADD X-AMT2 TO WS-DIRECT
           IF X-AMT2 NOT = 0
               MOVE "direct journals" TO X-NAME
           END-IF
           GENERATE DE-LINE.
       2100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-021  the proof itself.  The sales ledger balance is
      *> the arithmetic sum of every posted document; the categories
      *> above explain what it is made of.
      *>----------------------------------------------------------------
       3000-PROOF SECTION.
       3000-START.
           PERFORM 8800-PUT-GAP
           MOVE "Reconciliation proof" TO X-LABEL
           MOVE SPACES TO X-REF X-DATE X-NAME
           PERFORM 8700-PUT-TEXT
           PERFORM 8750-PUT-RULE
           PERFORM 3100-PROOF-LINES
           COMPUTE K-DIFF = K-LEDGER - K-GL
           MOVE SPACES TO X-LABEL X-REF X-DATE X-NAME
           MOVE "Difference (ledger less control)" TO X-LABEL
           MOVE 0 TO X-AMT1
           MOVE K-DIFF TO X-AMT2
           GENERATE DE-LINE
           PERFORM 8750-PUT-RULE
           IF K-DIFF > 0.005 OR K-DIFF < -0.005
               MOVE 4 TO WS-RC
               MOVE SPACES TO X-LABEL X-REF X-DATE X-NAME
               MOVE "OUT OF BALANCE - an exception has been"
                   TO X-LABEL
               MOVE "raised for the owner" TO X-REF
               PERFORM 8700-PUT-TEXT
               IF WS-DIRECT NOT = 0
                   MOVE SPACES TO X-LABEL X-REF X-DATE X-NAME
                   MOVE "Journals posted direct to a control"
                       TO X-LABEL
                   MOVE "account" TO X-REF
                   MOVE WS-DIRECT TO X-AMT2
                   MOVE 0 TO X-AMT1
                   GENERATE DE-LINE
               END-IF
           ELSE
               MOVE SPACES TO X-LABEL X-REF X-DATE X-NAME
               MOVE "Sales ledger agrees with the general"
                   TO X-LABEL
               MOVE "ledger control account" TO X-REF
               PERFORM 8700-PUT-TEXT
           END-IF.
       3000-EXIT.
           EXIT.

       3100-PROOF-LINES SECTION.
       3100-START.
           MOVE SPACES TO X-LABEL X-REF X-DATE X-NAME
           MOVE "Outstanding invoices and debit items" TO X-LABEL
           MOVE K-DEBT TO X-AMT1
           MOVE 0 TO X-AMT2
           GENERATE DE-LINE
           MOVE SPACES TO X-LABEL
           MOVE "less unapplied credit notes" TO X-LABEL
           MOVE K-CREDIT TO X-AMT1
           GENERATE DE-LINE
           MOVE SPACES TO X-LABEL
           MOVE "less cash on account (unapplied)" TO X-LABEL
           MOVE K-CASH TO X-AMT1
           GENERATE DE-LINE
           MOVE SPACES TO X-LABEL
           MOVE "less customer deposits held" TO X-LABEL
           MOVE K-DEPOSIT TO X-AMT1
           GENERATE DE-LINE
           MOVE SPACES TO X-LABEL
           MOVE "adjustments, allowances and write offs" TO X-LABEL
           MOVE K-ALLOW TO X-AMT1
           GENERATE DE-LINE
           PERFORM 8750-PUT-RULE
           MOVE SPACES TO X-LABEL
           MOVE "Sales ledger balance" TO X-LABEL
           MOVE 0 TO X-AMT1
           MOVE K-LEDGER TO X-AMT2
           GENERATE DE-LINE
           MOVE SPACES TO X-LABEL
           MOVE "General ledger control account balance" TO X-LABEL
           MOVE K-GL TO X-AMT2
           GENERATE DE-LINE.
       3100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-020  credits, write offs, transfers and corrections
      *> stay distinguishable in the customer history
      *>----------------------------------------------------------------
       4000-ADJUSTMENTS SECTION.
       4000-START.
           PERFORM 8800-PUT-GAP
           MOVE "Adjustments and write offs to the date" TO X-LABEL
           MOVE "Document" TO X-REF
           MOVE "Posted" TO X-DATE
           MOVE "Customer" TO X-NAME
           PERFORM 8700-PUT-TEXT
           PERFORM 8750-PUT-RULE
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 200 o.DocumentNumber, o.ItemType, "
                  "o.CustomerCode, CONVERT(char(8),o.PostingDate,"
                  "112), CONVERT(varchar(24),o.OriginalAmountGBP), "
                  "ISNULL(o.CreatedBy,'') FROM ar.OpenItem o WHERE "
                  "o.CompanyCode = ? AND o.ItemType IN "
                  "('ADJUSTMENT','WRITEOFF','INTEREST') AND "
                  "o.PostingDate <= ? ORDER BY o.PostingDate DESC, "
                  "o.DocumentNumber"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-ASAT TO AR-DATE-ED
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 6 TO SQLIO-COL-CNT
           PERFORM 8000-SET-WIDTHS
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               GO TO 4000-EXIT
           END-IF
           MOVE "N" TO AR-DONE
           PERFORM UNTIL AR-FINISHED
               MOVE 3 TO SQLIO-CURSOR
               MOVE 6 TO SQLIO-COL-CNT
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO AR-DONE
               ELSE
                   PERFORM 4100-ONE-ADJUSTMENT
               END-IF
           END-PERFORM
           MOVE 3 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF WS-ADJ-CNT = 0
               MOVE SPACES TO X-LABEL X-REF X-DATE X-NAME
               MOVE "No adjustments or write offs recorded"
                   TO X-LABEL
               PERFORM 8700-PUT-TEXT
           END-IF
           PERFORM 8750-PUT-RULE.
       4000-EXIT.
           EXIT.

       4100-ONE-ADJUSTMENT SECTION.
       4100-START.
           ADD 1 TO WS-ADJ-CNT
           MOVE SPACES TO X-LABEL X-REF X-DATE X-NAME
           MOVE BR-COL(2) TO X-LABEL
           MOVE BR-COL(1) TO X-REF
           MOVE BR-COL(4) TO AR-COMPACT-DATE
           COMPUTE AR-NUM-DATE = FUNCTION NUMVAL(AR-COMPACT-DATE)
           PERFORM 8100-FORMAT-DATE
           MOVE WS-UK TO X-DATE
           MOVE BR-COL(3) TO X-NAME
           COMPUTE X-AMT1 = FUNCTION NUMVAL(BR-COL(5))
           MOVE 0 TO X-AMT2
           GENERATE DE-LINE.
       4100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-021  an out of balance close is owned work
      *>----------------------------------------------------------------
       5000-RAISE-DIFFERENCE SECTION.
       5000-START.
           INITIALIZE WS-EXCPRAIS
           MOVE "RAISE" TO CA-FUNCTION OF WS-EXCPRAIS
           MOVE WS-CO TO CA-COMPANY OF WS-EXCPRAIS
           MOVE WS-USER TO CA-USER OF WS-EXCPRAIS
           MOVE WS-ASAT TO CA-BUSINESS-DATE OF WS-EXCPRAIS
           MOVE "AR" TO EX-MODULE
           MOVE "WARNING" TO EX-SEVERITY
           MOVE "CONTROL ACCOUNT DIFFERENCE" TO EX-CATEGORY
           MOVE "RECONCILIATION" TO EX-DOCUMENT-TYPE
           IF WS-PERIOD = SPACES
               MOVE WS-ASAT TO AR-DATE-ED
               MOVE AR-DATE-ED TO EX-DOCUMENT-REF
           ELSE
               MOVE WS-PERIOD TO EX-DOCUMENT-REF
           END-IF
           MOVE WS-USER TO EX-OWNER-USER
           MOVE "FINCTRL" TO EX-OWNER-ROLE
           MOVE 1 TO EX-PRIORITY
           MOVE WS-ASAT TO EX-DUE-AT
           MOVE K-DIFF TO EX-AMOUNT-GBP
           MOVE K-DIFF TO AR-MONEY-UK
           MOVE SPACES TO EX-MESSAGE-TEXT
           STRING "Sales ledger and AR control account differ by "
                  "GBP " FUNCTION TRIM(AR-MONEY-UK) " as at "
                  WS-UK " - see the ARRECON proof"
               DELIMITED BY SIZE INTO EX-MESSAGE-TEXT
           CALL "EXCPRAIS" USING WS-EXCPRAIS
           IF CA-RETURN-CODE OF WS-EXCPRAIS > 6
               DISPLAY "STERP-W-5001 exception not raised"
           END-IF.
       5000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-SET-WIDTHS SECTION.
       8000-START.
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 20
               MOVE 120 TO SQLIO-COL-WIDTH(AR-I)
           END-PERFORM.
       8000-EXIT.
           EXIT.

       8100-FORMAT-DATE SECTION.
       8100-START.
           MOVE AR-NUM-DATE TO AR-DATE-ED
           COMPUTE WS-UK-YYYY = FUNCTION NUMVAL(AR-DATE-ED(1:4))
           COMPUTE WS-UK-MM = FUNCTION NUMVAL(AR-DATE-ED(5:2))
           COMPUTE WS-UK-DD = FUNCTION NUMVAL(AR-DATE-ED(7:2)).
       8100-EXIT.
           EXIT.

       8700-PUT-TEXT SECTION.
       8700-START.
           GENERATE DE-TEXT
           MOVE SPACES TO X-LABEL X-REF X-DATE X-NAME.
       8700-EXIT.
           EXIT.

       8750-PUT-RULE SECTION.
       8750-START.
           GENERATE DE-RULE.
       8750-EXIT.
           EXIT.

       8800-PUT-GAP SECTION.
       8800-START.
           GENERATE DE-GAP.
       8800-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-ARRECON
           CLOSE RPT-FILE
           MOVE K-DIFF TO AR-MONEY-ED
           DISPLAY "STERP-I-0401 difference GBP "
                   FUNCTION TRIM(AR-MONEY-ED)
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0400-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "ARRECON" TO JL-JOB-NAME
           MOVE WS-JOBID TO JL-JOB-RUN-ID
           EVALUATE TRUE
               WHEN WS-RC = 0   MOVE "COMPLETE" TO JL-STATUS
               WHEN WS-RC = 4   MOVE "WARNING" TO JL-STATUS
               WHEN OTHER       MOVE "FAILED" TO JL-STATUS
           END-EVALUATE
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE WS-ROWS-OUT TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       0500-EXIT.
           EXIT.
