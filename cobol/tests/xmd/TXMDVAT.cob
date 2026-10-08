      *>================================================================
      *> PROGRAM-ID : TXMDVAT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : CROSS MODULE CONTROLS (XMD) UNIT TESTS
      *> PURPOSE    : VATAUDIT - THE VAT DETAIL AND MTD AUDIT TRAIL
      *>              LISTING.  BOTH MODES ARE RUN AGAINST COMPANY
      *>              STC AND A SEEDED VAT PERIOD AND MUST COME BACK
      *>              00 OR 04 (04 BEING A PRINTED, OWNED DIFFERENCE
      *>              - NEVER A CRASH).  THE DETAIL RUN MUST LIST
      *>              EVERY fin.VatTransaction OF THE PERIOD, AND
      *>              THE MTD RUN MUST ECHO NINE BOX VALUES THAT
      *>              AGREE WITH fin.VatReturnBox TO THE PENNY - THE
      *>              DIGITAL LINK IS ONLY WORTH ANYTHING IF THE
      *>              LISTING AND THE STORED RETURN CANNOT DRIFT.
      *>              AN UNKNOWN VAT PERIOD IS REFUSED WITH 08.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: RPT-007, RPT-017, XMD-005, XMD-006, COM-017, COM-020
      *> AC: VATAUDIT MODE=DETAIL and MODE=MTD return 00 or 04 for
      *> AC: company STC and VAT period 26A3
      *> AC: the detail listing counts one line per VAT transaction
      *> AC: the nine MTD box values equal fin.VatReturnBox.BoxValue
      *> AC: an unknown VAT period is refused with 08, not guessed
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXMDVAT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CAP-FILE ASSIGN TO DYNAMIC WS-CAP-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS.
       DATA DIVISION.
       FILE SECTION.
       FD  CAP-FILE.
       01  CAP-REC                     PIC X(200).
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       01  WS-ROW.
           05  WS-COL OCCURS 10 TIMES  PIC X(120).
       01  WS-CAP-NAME                 PIC X(250) VALUE SPACES.
       01  WS-FS                       PIC X(2) VALUE "00".
       01  WS-LINE                     PIC X(200) VALUE SPACES.
       01  WS-WORK.
           05  WS-DB-UP                PIC X VALUE "N".
           05  WS-READY                PIC X VALUE "N".
           05  WS-EOF                  PIC X VALUE "N".
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-K                    PIC S9(4) VALUE 0.
           05  WS-N                    PIC S9(7) VALUE 0.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-ROOT                 PIC X(200) VALUE SPACES.
           05  WS-CMD                  PIC X(400) VALUE SPACES.
           05  WS-CO                   PIC X(10) VALUE "STC".
           05  WS-PERIOD               PIC X(10) VALUE "26A3".
           05  WS-RETID                PIC X(12) VALUE SPACES.
           05  WS-TXNS                 PIC S9(7) VALUE 0.
           05  WS-ROWS                 PIC S9(7) VALUE 0.
           05  WS-SPOOL-SEEN           PIC X VALUE "N".
           05  WS-BOXES-SEEN           PIC S9(4) VALUE 0.
           05  WS-BOXNO                PIC S9(4) VALUE 0.
           05  WS-AMT                  PIC S9(13)V99 VALUE 0.
           05  WS-DB-AMT               PIC S9(13)V99 VALUE 0.
           05  WS-SEQ-ED               PIC 9(4) VALUE 0.
           05  WS-NAME-X               PIC X(100) VALUE SPACES.
       01  WS-SEEN.
           05  WS-BOX OCCURS 9 TIMES.
               10  WS-BOX-VAL          PIC S9(13)V99.
               10  WS-BOX-FLAG         PIC X.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TXMDVAT VAT audit trail listing" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               MOVE "VAT audit trail (no database)" TO TK-NAME
               MOVE "SQL Server is not reachable" TO TK-DETAIL
               PERFORM TK-SKIP
           ELSE
               PERFORM 0200-FIND-ROOT
               PERFORM 1000-FIXTURE THRU 1000-EXIT
               IF WS-READY = "N"
                   MOVE "VAT audit trail (period not seeded)"
                       TO TK-NAME
                   MOVE "fin.VatPeriod 26A3 for STC is missing"
                       TO TK-DETAIL
                   PERFORM TK-SKIP
               ELSE
                   PERFORM 2000-DETAIL-MODE
                   PERFORM 3000-MTD-MODE
                   PERFORM 4000-BAD-PERIOD
               END-IF
           END-IF
           PERFORM TK-END
           STOP RUN.

       0100-PROBE-DATABASE.
           MOVE "N" TO WS-DB-UP
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                  "core.Company"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8100-ONE-COUNT
           IF SQLIO-OK
               MOVE "Y" TO WS-DB-UP
           END-IF.

      *>   the harness runs from bin\tests, so the root of the tree
      *>   is taken from %STERP_RUN% rather than assumed
       0200-FIND-ROOT.
           MOVE SPACES TO WS-ROOT
           ACCEPT WS-ROOT FROM ENVIRONMENT "STERP_RUN"
           IF WS-ROOT = SPACES
               MOVE "." TO WS-ROOT
           ELSE
               MOVE SPACES TO WS-CMD
               STRING FUNCTION TRIM(WS-ROOT) "\.."
                   DELIMITED BY SIZE INTO WS-CMD
               MOVE WS-CMD(1:200) TO WS-ROOT
           END-IF.

      *>----------------------------------------------------------------
      *> REQ: RPT-017  the fixture - a VAT period of STC must carry a
      *> return and the nine stored boxes before the MTD listing can
      *> be proved against them.  both inserts are guarded, so a
      *> return prepared by the finance module is never overwritten
      *>----------------------------------------------------------------
       1000-FIXTURE.
           MOVE "N" TO WS-READY
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                  "fin.VatPeriod WHERE CompanyCode = '"
                  FUNCTION TRIM(WS-CO) "' AND VatPeriodKey = '"
                  FUNCTION TRIM(WS-PERIOD) "'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8100-ONE-COUNT
           IF WS-N = 0
               GO TO 1000-EXIT
           END-IF
           PERFORM 1100-SEED-RETURN
           PERFORM 1200-READ-RETURN-ID
           IF WS-RETID = SPACES
               GO TO 1000-EXIT
           END-IF
           PERFORM 1300-SEED-BOXES
           PERFORM 1400-COUNT-TXNS
           MOVE "Y" TO WS-READY.
       1000-EXIT.
           EXIT.

       1100-SEED-RETURN.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO fin.VatReturn (CompanyCode,"
                  "VatPeriodKey,Version,VatRegNo,Box1,Box2,Box3,"
                  "Box4,Box5,Box6,Box7,Box8,Box9,Status,PreparedBy"
                  ") SELECT p.CompanyCode,p.VatPeriodKey,1,"
                  "p.VatRegNo,t.B1,0,t.B1,t.B4,t.B1-t.B4,t.B6,t.B7,"
                  "0,0,'DRAFT','TXMDVAT' FROM fin.VatPeriod p CROSS"
                  " APPLY (SELECT ISNULL(SUM(CASE WHEN v.Direction "
                  "= 'OUTPUT' THEN v.VatAmountGBP ELSE 0 END),0) AS"
                  " B1, ISNULL(SUM(CASE WHEN v.Direction <> "
                  "'OUTPUT' THEN ISNULL(v.RecoverableVatGBP,"
                  "v.VatAmountGBP) ELSE 0 END),0) AS B4, ISNULL("
                  "SUM(CASE WHEN v.Direction = 'OUTPUT' THEN "
                  "v.NetAmountGBP ELSE 0 END),0) AS B6, ISNULL(SUM("
                  "CASE WHEN v.Direction <> 'OUTPUT' THEN "
                  "v.NetAmountGBP ELSE 0 END),0) AS B7 FROM "
                  "fin.VatTransaction v WHERE v.CompanyCode = "
                  "p.CompanyCode AND v.VatPeriodKey = "
                  "p.VatPeriodKey) t WHERE p.CompanyCode = ? AND "
                  "p.VatPeriodKey = ? AND NOT EXISTS (SELECT 1 FROM"
                  " fin.VatReturn r WHERE r.CompanyCode = "
                  "p.CompanyCode AND r.VatPeriodKey = "
                  "p.VatPeriodKey)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-PERIOD TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

       1200-READ-RETURN-ID.
           MOVE SPACES TO WS-RETID
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 CONVERT(varchar(12),r.VatReturnId)"
                  " FROM fin.VatReturn r WHERE r.CompanyCode = ? "
                  "AND r.VatPeriodKey = ? ORDER BY r.Version DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-PERIOD TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-OK
               MOVE WS-COL(1)(1:12) TO WS-RETID
           END-IF.

       1300-SEED-BOXES.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO fin.VatReturnBox (VatReturnId,"
                  "CompanyCode,BoxNo,BoxValue,TransactionCount,"
                  "IsWholePounds,Caption) SELECT r.VatReturnId,"
                  "r.CompanyCode,b.BoxNo,CASE b.BoxNo WHEN 1 THEN "
                  "r.Box1 WHEN 2 THEN r.Box2 WHEN 3 THEN r.Box3 "
                  "WHEN 4 THEN r.Box4 WHEN 5 THEN r.Box5 WHEN 6 "
                  "THEN r.Box6 WHEN 7 THEN r.Box7 WHEN 8 THEN "
                  "r.Box8 ELSE r.Box9 END,0,0,'Box ' + CONVERT("
                  "varchar(2),b.BoxNo) FROM fin.VatReturn r CROSS "
                  "JOIN (VALUES (1),(2),(3),(4),(5),(6),(7),(8),(9)"
                  ") b(BoxNo) WHERE r.VatReturnId = ? AND NOT "
                  "EXISTS (SELECT 1 FROM fin.VatReturnBox x WHERE "
                  "x.VatReturnId = r.VatReturnId AND x.BoxNo = "
                  "b.BoxNo)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-RETID TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

       1400-COUNT-TXNS.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                  "fin.VatTransaction WHERE CompanyCode = '"
                  FUNCTION TRIM(WS-CO) "' AND VatPeriodKey = '"
                  FUNCTION TRIM(WS-PERIOD) "'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8100-ONE-COUNT
           MOVE WS-N TO WS-TXNS.

      *>----------------------------------------------------------------
      *> REQ: RPT-007, COM-017  the detail listing runs clean and
      *> accounts for every VAT transaction of the period.  a
      *> printed difference is 04, which is a warning, not a failure
      *>----------------------------------------------------------------
       2000-DETAIL-MODE.
           MOVE "DETAIL" TO WS-NAME-X
           PERFORM 8200-RUN-JOB
           MOVE "the detail listing runs and returns 00 or 04"
               TO TK-NAME
           IF WS-RC = 0 OR WS-RC = 4
               PERFORM TK-PASS
           ELSE
               MOVE WS-RC TO TK-ACTUAL-N
               MOVE 0 TO TK-EXPECTED-N
               MOVE "VATAUDIT MODE=DETAIL did not come back 00/04"
                   TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           PERFORM 8300-SCAN-CAPTURE THRU 8300-EXIT
           MOVE "the detail listing names its spool file" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE WS-SPOOL-SEEN TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "one listed line per VAT transaction of the period"
               TO TK-NAME
           MOVE WS-TXNS TO TK-EXPECTED-N
           MOVE WS-ROWS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> REQ: RPT-017  the MTD listing runs clean and the nine boxes
      *> it prints are the nine boxes held on fin.VatReturnBox - the
      *> digital link between the ledger and the figures filed
      *>----------------------------------------------------------------
       3000-MTD-MODE.
           MOVE "MTD" TO WS-NAME-X
           PERFORM 8200-RUN-JOB
           MOVE "the MTD audit trail runs and returns 00 or 04"
               TO TK-NAME
           IF WS-RC = 0 OR WS-RC = 4
               PERFORM TK-PASS
           ELSE
               MOVE WS-RC TO TK-ACTUAL-N
               MOVE 0 TO TK-EXPECTED-N
               MOVE "VATAUDIT MODE=MTD did not come back 00/04"
                   TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           PERFORM 8300-SCAN-CAPTURE THRU 8300-EXIT
           MOVE "the MTD audit trail names its spool file" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE WS-SPOOL-SEEN TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "all nine VAT return boxes are listed" TO TK-NAME
           MOVE 9 TO TK-EXPECTED-N
           MOVE WS-BOXES-SEEN TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 3100-COMPARE-BOXES
               VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9.

       3100-COMPARE-BOXES.
           MOVE WS-I TO WS-SEQ-ED
           MOVE SPACES TO TK-NAME
           STRING "box " WS-SEQ-ED(3:2)
                  " of the listing equals fin.VatReturnBox"
               DELIMITED BY SIZE INTO TK-NAME
           PERFORM 8400-READ-BOX
           IF WS-BOX-FLAG(WS-I) NOT = "Y"
               MOVE "the listing did not echo this box"
                   TO TK-DETAIL
               MOVE 1 TO TK-EXPECTED-N
               MOVE 0 TO TK-ACTUAL-N
               PERFORM TK-FAIL
           ELSE
               MOVE WS-DB-AMT TO TK-EXPECTED-N
               MOVE WS-BOX-VAL(WS-I) TO TK-ACTUAL-N
               PERFORM TK-ASSERT-NUM
           END-IF.

       8400-READ-BOX.
           MOVE 0 TO WS-DB-AMT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24),b.BoxValue) FROM "
                  "fin.VatReturnBox b WHERE b.VatReturnId = ? AND "
                  "b.BoxNo = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-RETID TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-I TO WS-SEQ-ED
           MOVE WS-SEQ-ED TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-OK
               COMPUTE WS-DB-AMT = FUNCTION NUMVAL(WS-COL(1))
           END-IF.

      *>----------------------------------------------------------------
      *> REQ: COM-017  a VAT period that does not exist is refused
      *> with 08 and the job still logs its end - it never crashes
      *>----------------------------------------------------------------
       4000-BAD-PERIOD.
           MOVE "NOSUCH" TO WS-NAME-X
           PERFORM 8200-RUN-JOB
           MOVE "an unknown VAT period is refused with 08"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE WS-RC TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   WS-NAME-X carries DETAIL, MTD or NOSUCH; the standard
      *>   output of the job is captured so that the listing can be
      *>   proved against the database it was built from
       8200-RUN-JOB.
           MOVE SPACES TO WS-CAP-NAME
           STRING FUNCTION TRIM(WS-ROOT) "\run\spool\TXMDVAT-"
                  FUNCTION TRIM(WS-NAME-X) ".out"
               DELIMITED BY SIZE INTO WS-CAP-NAME
           MOVE SPACES TO WS-CMD
           IF WS-NAME-X = "NOSUCH"
               STRING "cd /d """ FUNCTION TRIM(WS-ROOT) """ && "
                      "(if not exist run\spool md run\spool) && "
                      "bin\x86\VATAUDIT.exe CO=STC VATPERIOD=99Z9 "
                      "MODE=MTD USER=FINCTRL > run\spool\"
                      "TXMDVAT-NOSUCH.out"
                   DELIMITED BY SIZE INTO WS-CMD
           ELSE
               STRING "cd /d """ FUNCTION TRIM(WS-ROOT) """ && "
                      "(if not exist run\spool md run\spool) && "
                      "bin\x86\VATAUDIT.exe CO=" FUNCTION TRIM(WS-CO)
                      " VATPERIOD=" FUNCTION TRIM(WS-PERIOD)
                      " MODE=" FUNCTION TRIM(WS-NAME-X)
                      " USER=FINCTRL > run\spool\TXMDVAT-"
                      FUNCTION TRIM(WS-NAME-X) ".out"
                   DELIMITED BY SIZE INTO WS-CMD
           END-IF
           MOVE 0 TO WS-RC
           CALL "SYSTEM" USING WS-CMD
           MOVE RETURN-CODE TO WS-RC
           MOVE 0 TO RETURN-CODE.

      *>   read back what the job said about itself: the spool path,
      *>   the number of detail lines and the nine box echoes
       8300-SCAN-CAPTURE.
           MOVE "N" TO WS-SPOOL-SEEN
           MOVE 0 TO WS-ROWS
           MOVE 0 TO WS-BOXES-SEEN
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
               MOVE 0 TO WS-BOX-VAL(WS-I)
               MOVE "N" TO WS-BOX-FLAG(WS-I)
           END-PERFORM
           MOVE "N" TO WS-EOF
           MOVE "00" TO WS-FS
           OPEN INPUT CAP-FILE
           IF WS-FS NOT = "00"
               GO TO 8300-EXIT
           END-IF
           PERFORM UNTIL WS-EOF = "Y"
               READ CAP-FILE INTO WS-LINE
                   AT END MOVE "Y" TO WS-EOF
                   NOT AT END PERFORM 8350-ONE-LINE
               END-READ
           END-PERFORM
           CLOSE CAP-FILE.
       8300-EXIT.
           EXIT.

       8350-ONE-LINE.
           EVALUATE TRUE
               WHEN WS-LINE(1:14) = "STERP-I-SPOOL "
                   MOVE "Y" TO WS-SPOOL-SEEN
               WHEN WS-LINE(1:16) = "STERP-I-VATROWS "
                   COMPUTE WS-ROWS =
                       FUNCTION NUMVAL(WS-LINE(17:9))
               WHEN WS-LINE(1:15) = "STERP-I-MTDBOX "
                   PERFORM 8360-ONE-BOX THRU 8360-EXIT
           END-EVALUATE
           MOVE SPACES TO WS-LINE.

       8360-ONE-BOX.
           COMPUTE WS-BOXNO = FUNCTION NUMVAL(WS-LINE(16:2))
           IF WS-BOXNO < 1 OR WS-BOXNO > 9
               GO TO 8360-EXIT
           END-IF
           COMPUTE WS-AMT = FUNCTION NUMVAL(WS-LINE(19:18))
           MOVE WS-AMT TO WS-BOX-VAL(WS-BOXNO)
           MOVE "Y" TO WS-BOX-FLAG(WS-BOXNO)
           ADD 1 TO WS-BOXES-SEEN.
       8360-EXIT.
           EXIT.

       8100-ONE-COUNT.
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE 0 TO WS-N
           IF SQLIO-OK
               COMPUTE WS-N = FUNCTION NUMVAL(WS-COL(1))
           END-IF.

       8000-WIDTHS.
           PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > 10
               MOVE 120 TO SQLIO-COL-WIDTH(WS-K)
           END-PERFORM.

       COPY TESTKITP.
