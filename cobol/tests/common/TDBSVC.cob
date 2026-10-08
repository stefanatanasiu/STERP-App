      *>================================================================
      *> PROGRAM  : TDBSVC
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : COMMON SERVICES - UNIT TEST
      *> PURPOSE  : Single driver for the database backed common
      *>            services: PARMGET, NUMGEN, PERIODCK, PERMCHK,
      *>            AUDITLOG, EXCPRAIS, IDEMPOT and JOBLOG.  One driver
      *>            rather than eight keeps the fixture rows and the
      *>            enclosing transaction in one place.
      *> CALLED BY: build\run-tests.ps1 -Layer cobol
      *> INTERFACE: TAP on stdout, RETURN-CODE = number of failures.
      *>
      *> REPEATABILITY (CONVENTIONS 3.7).  The suite runs in two phases.
      *>   PHASE A runs inside one SQLIO BEGN ... ROLB transaction and
      *>     holds every test that writes.  It inserts its own number
      *>     series and parameter rows, under keys prefixed ZZ /
      *>     STERP.TEST that no seed uses, so the assertions do not
      *>     depend on how far the core seed has got.  The rollback
      *>     leaves the database exactly as it was found.
      *>   PHASE B runs afterwards, outside any transaction, and holds
      *>     the refusal cases.  Several core procedures report a
      *>     business refusal with THROW, and because they run with SET
      *>     XACT_ABORT ON that also dooms the caller's transaction.
      *>     Keeping the refusals in their own phase stops one expected
      *>     refusal from destroying the fixtures of every later test.
      *>
      *> If the database is not reachable the whole suite reports TAP
      *> "# SKIP" instead of failing, so that a developer without a
      *> local SQL Server still gets a green COBOL build.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2026-10-05 STERP INITIAL - COVER THE DB BACKED COMMON SERVICES
      *>================================================================
      *> REQ: COM-002, COM-005, COM-009, COM-011, COM-013, COM-014
      *> REQ: COM-015, COM-017, COM-018, COM-019, COM-022, COM-025
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TDBSVC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY PARMGETCA  REPLACING ==DFHCOMMAREA== BY ==WS-PARM==.
       COPY NUMGENCA   REPLACING ==DFHCOMMAREA== BY ==WS-NUMG==.
       COPY PERIODCKCA REPLACING ==DFHCOMMAREA== BY ==WS-PERC==.
       COPY PERMCHKCA  REPLACING ==DFHCOMMAREA== BY ==WS-PERM==.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUDT==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXCP==.
       COPY IDEMPOTCA  REPLACING ==DFHCOMMAREA== BY ==WS-IDEM==.
       COPY JOBLOGCA   REPLACING ==DFHCOMMAREA== BY ==WS-JOBL==.

       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "Y".
           05  WS-TRAN-OPEN            PIC X VALUE "N".
       01  WS-PROBE.
           05  WS-PROBE-COL            PIC X(12).
       01  WS-WORK.
           05  WS-DOC-1                PIC X(30) VALUE SPACES.
           05  WS-DOC-2                PIC X(30) VALUE SPACES.
           05  WS-SEQ-1                PIC S9(9) COMP-5 VALUE 0.
           05  WS-CORR-A               PIC X(36) VALUE SPACES.
           05  WS-CORR-B               PIC X(36) VALUE SPACES.
           05  WS-JOB-RUN-ID           PIC S9(9) COMP-5 VALUE 0.
           05  WS-I                    PIC S9(4) COMP-5 VALUE 0.

      *> Fixture keys - deliberately outside the seeded ranges.
      *> The dates follow the reference calendar of CONVENTIONS 4.7:
      *> fiscal year April-March, April = P01, so October 2026 is
      *> fiscal 2026 period 7 (OPEN), September 2026 period 6 (SOFT)
      *> and May 2026 period 2 (HARD closed).
       01  WS-FIX.
           05  WF-COMPANY              PIC X(10) VALUE "STC".
           05  WF-SITE                 PIC X(10) VALUE "RDG".
           05  WF-USER                 PIC X(20) VALUE "SYSADM".
           05  WF-PARAM                PIC X(40)
                                       VALUE "STERP.TEST.SETTLEDAYS".
           05  WF-SERIES               PIC X(20) VALUE "ZZTEST".
           05  WF-OPEN-DATE            PIC 9(8) VALUE 20261015.
           05  WF-SOFT-DATE            PIC 9(8) VALUE 20260915.
           05  WF-HARD-DATE            PIC 9(8) VALUE 20260515.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TDBSVC" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-EVERYTHING
           ELSE
      *>         PHASE A - everything that writes, inside a transaction
               PERFORM 0200-START-TRANSACTION
               PERFORM 0300-INSERT-FIXTURES
               PERFORM 1000-PARMGET-EFFECTIVE
               PERFORM 2000-NUMGEN-ISSUE
               PERFORM 3000-PERIODCK-STATUS
               PERFORM 4000-PERMCHK-DECISION
               PERFORM 5000-AUDITLOG-WRITE
               PERFORM 6000-EXCPRAIS-RAISE
               PERFORM 7000-IDEMPOT-CYCLE
               PERFORM 8000-JOBLOG-CYCLE
               PERFORM 0800-ROLL-BACK
      *>         PHASE B - refusals, outside any transaction
               PERFORM 9100-INPUT-REFUSALS
               PERFORM 9200-BUSINESS-REFUSALS
           END-IF
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
      *> 0100 - is there a database?  A failure here is not a test
      *> failure: the DB may be rebuilding (see CONVENTIONS 4.2).
      *>----------------------------------------------------------------
       0100-PROBE-DATABASE.
           MOVE "SNGL" TO SQLIO-FUNC
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SELECT CAST(1 AS varchar(10))" TO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF NOT SQLIO-OK
               MOVE "N" TO WS-DB-UP
           END-IF.

       0200-START-TRANSACTION.
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-OK
               MOVE "Y" TO WS-TRAN-OPEN
           END-IF
           MOVE "SQLIO BEGN opens a transaction for the suite"
               TO TK-NAME
           IF WS-TRAN-OPEN = "Y"
               PERFORM TK-PASS
           ELSE
               MOVE SQLIO-ERRMSG TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

       0800-ROLL-BACK.
           IF WS-TRAN-OPEN = "Y"
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-PROBE
               MOVE "SQLIO ROLB leaves no trace of the fixtures"
                   TO TK-NAME
               IF SQLIO-OK
                   PERFORM TK-PASS
               ELSE
                   MOVE SQLIO-ERRMSG TO TK-DETAIL
                   PERFORM TK-FAIL
               END-IF
               MOVE "N" TO WS-TRAN-OPEN
           END-IF.

       0900-SKIP-EVERYTHING.
           MOVE "database not reachable - STERP_ODBC / SQL Server"
               TO TK-DETAIL
           MOVE "TDBSVC database backed services" TO TK-NAME
           PERFORM TK-SKIP.

      *>----------------------------------------------------------------
      *> 0300 - fixture rows.  All inserts are parameterised; no user
      *> text is ever concatenated into SQL (CONVENTIONS 3.4).
      *>----------------------------------------------------------------
       0300-INSERT-FIXTURES.
           PERFORM 0310-FIXTURE-PARAMETER
           PERFORM 0320-FIXTURE-SERIES.

       0310-FIXTURE-PARAMETER.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO core.Parameter (ParamCode, "
                  "CompanyCode, EffectiveFrom, Value, DataType, "
                  "UnitOfValue, DecisionRef, Description, OwnerRole) "
                  "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE "EXEC" TO SQLIO-FUNC
           MOVE 1 TO SQLIO-CURSOR
           MOVE 9 TO SQLIO-PARM-CNT
           MOVE WF-PARAM TO SQLIO-PARM-VAL(1)
           MOVE WF-COMPANY TO SQLIO-PARM-VAL(2)
           MOVE "20200101" TO SQLIO-PARM-VAL(3)
           MOVE "42.5" TO SQLIO-PARM-VAL(4)
           MOVE "DECIMAL" TO SQLIO-PARM-VAL(5)
           MOVE "DAYS" TO SQLIO-PARM-VAL(6)
           MOVE "D-09" TO SQLIO-PARM-VAL(7)
           MOVE "TDBSVC fixture - settlement days" TO SQLIO-PARM-VAL(8)
           MOVE "FINCTRL" TO SQLIO-PARM-VAL(9)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           MOVE "fixture: core.Parameter row inserted" TO TK-NAME
           IF SQLIO-OK
               PERFORM TK-PASS
           ELSE
               MOVE SQLIO-ERRMSG TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

       0320-FIXTURE-SERIES.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO core.NumberSeries (CompanyCode, "
                  "SeriesCode, DocumentType, Description, Prefix, "
                  "NumberLength, NextNumber, MinNumber, MaxNumber, "
                  "IsLegalSequence, IsActive) "
                  "VALUES (?, ?, ?, ?, ?, 6, 1, 1, 999999, 1, 1)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE "EXEC" TO SQLIO-FUNC
           MOVE 1 TO SQLIO-CURSOR
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE WF-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE WF-SERIES TO SQLIO-PARM-VAL(2)
           MOVE "TEST" TO SQLIO-PARM-VAL(3)
           MOVE "TDBSVC fixture series" TO SQLIO-PARM-VAL(4)
           MOVE "ZZ" TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           MOVE "fixture: core.NumberSeries row inserted" TO TK-NAME
           IF SQLIO-OK
               PERFORM TK-PASS
           ELSE
               MOVE SQLIO-ERRMSG TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> 1000 - PARMGET.  COM-011 tuning parameters are held as data
      *> with an effective date, never compiled in.
      *>----------------------------------------------------------------
      *> REQ: COM-011
       1000-PARMGET-EFFECTIVE.
           INITIALIZE WS-PARM
           MOVE "PARMGET" TO CA-PROGRAM OF WS-PARM
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM
           MOVE WF-COMPANY TO CA-COMPANY OF WS-PARM
           MOVE WF-USER TO CA-USER OF WS-PARM
           MOVE WF-OPEN-DATE TO CA-BUSINESS-DATE OF WS-PARM
           MOVE WF-PARAM TO PM-PARAM-CODE
           CALL "PARMGET" USING WS-PARM
           MOVE "PARMGET GETNUM reads the effective value" TO TK-NAME
           IF CA-RETURN-CODE OF WS-PARM = 0 AND PM-FOUND = "Y"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-PARM TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "PARMGET GETNUM converts the value to a number"
               TO TK-NAME
           MOVE 42.5 TO TK-EXPECTED-N
           MOVE PM-VALUE-NUM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PARMGET returns the decision reference" TO TK-NAME
           MOVE "D-09" TO TK-EXPECTED-X
           MOVE PM-DECISION-REF TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PARMGET reports the effective date it used" TO TK-NAME
           MOVE 20200101 TO TK-EXPECTED-N
           MOVE PM-EFFECTIVE-FROM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PARMGET marks a numeric parameter as numeric"
               TO TK-NAME
           IF PM-IS-NUMERIC = "Y"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> 2000 - NUMGEN.  COM-005 every document gets a gap free legal
      *> sequence; COM-015 repeating the same instruction must not burn
      *> a second number; D-15 an abandoned number is voided, not
      *> reused.
      *>----------------------------------------------------------------
      *> REQ: COM-005, COM-015
       2000-NUMGEN-ISSUE.
           INITIALIZE WS-NUMG
           MOVE "NUMGEN" TO CA-PROGRAM OF WS-NUMG
           MOVE "NEXT" TO CA-FUNCTION OF WS-NUMG
           MOVE WF-COMPANY TO CA-COMPANY OF WS-NUMG
           MOVE WF-USER TO CA-USER OF WS-NUMG
           MOVE WF-OPEN-DATE TO CA-BUSINESS-DATE OF WS-NUMG
           MOVE "TDBSVC-NUMGEN-0000000000000000000001"
               TO CA-CORRELATION-ID OF WS-NUMG
           MOVE WF-SERIES TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUMG
           MOVE NG-DOCUMENT-NUMBER TO WS-DOC-1
           MOVE NG-SEQUENCE TO WS-SEQ-1
           MOVE "NUMGEN NEXT issues a number from the series"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-NUMG = 0
              AND NG-DOCUMENT-NUMBER NOT = SPACES
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-NUMG TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "NUMGEN starts the fixture series at 1" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE WS-SEQ-1 TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "NUMGEN formats prefix ZZ with 6 digits" TO TK-NAME
           MOVE "ZZ000001" TO TK-EXPECTED-X
           MOVE NG-DOCUMENT-NUMBER TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *> Same correlation id again - the number already issued.
           MOVE "NEXT" TO CA-FUNCTION OF WS-NUMG
           MOVE SPACES TO NG-DOCUMENT-NUMBER
           MOVE 0 TO NG-SEQUENCE
           CALL "NUMGEN" USING WS-NUMG
           MOVE "NUMGEN replays the number for a repeated correlation"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-NUMG < 8
              AND NG-DOCUMENT-NUMBER = WS-DOC-1
               PERFORM TK-PASS
           ELSE
               MOVE NG-DOCUMENT-NUMBER TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

      *> A different instruction gets the next number - no gaps.
           MOVE "NEXT" TO CA-FUNCTION OF WS-NUMG
           MOVE "TDBSVC-NUMGEN-0000000000000000000002"
               TO CA-CORRELATION-ID OF WS-NUMG
           CALL "NUMGEN" USING WS-NUMG
           MOVE NG-DOCUMENT-NUMBER TO WS-DOC-2
           MOVE "NUMGEN issues consecutive numbers without a gap"
               TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           MOVE NG-SEQUENCE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

      *> VOID keeps the reference traceable and never reissues it.
           MOVE "VOID" TO CA-FUNCTION OF WS-NUMG
           MOVE WS-DOC-2 TO NG-DOCUMENT-NUMBER
           MOVE "Abandoned by the TDBSVC unit test" TO NG-REASON-TEXT
           CALL "NUMGEN" USING WS-NUMG
           MOVE "NUMGEN VOID records an abandoned number (D-15)"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-NUMG = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-NUMG TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "NUMGEN VOID marks the number as void" TO TK-NAME
           MOVE "VOID" TO TK-EXPECTED-X
           MOVE NG-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *> The next instruction still gets 3 - a void never reopens 2.
           MOVE "NEXT" TO CA-FUNCTION OF WS-NUMG
           MOVE "TDBSVC-NUMGEN-0000000000000000000003"
               TO CA-CORRELATION-ID OF WS-NUMG
           MOVE WF-SERIES TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUMG
           MOVE "NUMGEN never reissues a voided number" TO TK-NAME
           MOVE 3 TO TK-EXPECTED-N
           MOVE NG-SEQUENCE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 3000 - PERIODCK.  COM-009: a posting date must fall in an open
      *> accounting period.  Backdating does not authorise posting into
      *> a closed period - a SOFT period returns 06 because an
      *> authorised route exists (D-15: only the financial controller
      *> reopens a period), a HARD period is refused outright.
      *> Dates come from the reference calendar in CONVENTIONS 4.7.
      *>----------------------------------------------------------------
      *> REQ: COM-009
       3000-PERIODCK-STATUS.
           INITIALIZE WS-PERC
           MOVE "PERIODCK" TO CA-PROGRAM OF WS-PERC
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERC
           MOVE WF-COMPANY TO CA-COMPANY OF WS-PERC
           MOVE WF-USER TO CA-USER OF WS-PERC
           MOVE WF-OPEN-DATE TO CA-BUSINESS-DATE OF WS-PERC
           MOVE WF-OPEN-DATE TO PC-POSTING-DATE
           MOVE WF-OPEN-DATE TO PC-DOCUMENT-DATE
           MOVE WF-OPEN-DATE TO PC-TAX-POINT
           MOVE "POST" TO PC-ACTION
           CALL "PERIODCK" USING WS-PERC
           MOVE "PERIODCK allows posting into an OPEN period" TO TK-NAME
           IF CA-RETURN-CODE OF WS-PERC = 0 AND PC-ALLOWED = "Y"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-PERC TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "PERIODCK reports 15 Oct 2026 as fiscal 2026 period 7"
               TO TK-NAME
           IF PC-FISCAL-YEAR = 2026 AND PC-PERIOD-NO = 7
               PERFORM TK-PASS
           ELSE
               MOVE PC-PERIOD-YYYYPP TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "PERIODCK returns the period status" TO TK-NAME
           MOVE "OPEN" TO TK-EXPECTED-X
           MOVE PC-PERIOD-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *> A soft closed period: blocked for an ordinary clerk, but an
      *> authorised route exists.  ARCLERK may post receipts yet holds
      *> no close adjustment authority.
           INITIALIZE WS-PERC
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERC
           MOVE WF-COMPANY TO CA-COMPANY OF WS-PERC
           MOVE "ARCLERK" TO CA-USER OF WS-PERC
           MOVE WF-SOFT-DATE TO CA-BUSINESS-DATE OF WS-PERC
           MOVE WF-SOFT-DATE TO PC-POSTING-DATE
           MOVE "POST" TO PC-ACTION
           CALL "PERIODCK" USING WS-PERC
           MOVE "PERIODCK holds a SOFT closed period for authority"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-PERC = 6 AND PC-ALLOWED = "N"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-PERC TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "PERIODCK names the permission that would authorise it"
               TO TK-NAME
           MOVE "FIN.PERIOD.SOFTPOST" TO TK-EXPECTED-X
           MOVE PC-OVERRIDE-PERM TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *> D-15 - the financial controller does hold that authority, so
      *> the same date is a permitted close adjustment for FINCTRL.
           INITIALIZE WS-PERC
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERC
           MOVE WF-COMPANY TO CA-COMPANY OF WS-PERC
           MOVE "FINCTRL" TO CA-USER OF WS-PERC
           MOVE WF-SOFT-DATE TO CA-BUSINESS-DATE OF WS-PERC
           MOVE WF-SOFT-DATE TO PC-POSTING-DATE
           MOVE "POST" TO PC-ACTION
           CALL "PERIODCK" USING WS-PERC
           MOVE "PERIODCK lets the controller post a close adjustment"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-PERC = 0 AND PC-ALLOWED = "Y"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-PERC TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

      *> A hard closed period is refused outright.
           INITIALIZE WS-PERC
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERC
           MOVE WF-COMPANY TO CA-COMPANY OF WS-PERC
           MOVE WF-USER TO CA-USER OF WS-PERC
           MOVE WF-HARD-DATE TO CA-BUSINESS-DATE OF WS-PERC
           MOVE WF-HARD-DATE TO PC-POSTING-DATE
           MOVE "POST" TO PC-ACTION
           CALL "PERIODCK" USING WS-PERC
           MOVE "PERIODCK blocks posting into a HARD closed period"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-PERC = 8 AND PC-ALLOWED = "N"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-PERC TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> 4000 - PERMCHK.  COM-002 / COM-019: an unauthorised attempt
      *> has no business effect and is explained - return code 06 with
      *> the reason from the procedure, never a silent failure.
      *>----------------------------------------------------------------
      *> REQ: COM-002, COM-019
       4000-PERMCHK-DECISION.
           INITIALIZE WS-PERM
           MOVE "PERMCHK" TO CA-PROGRAM OF WS-PERM
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERM
           MOVE WF-COMPANY TO CA-COMPANY OF WS-PERM
           MOVE WF-USER TO CA-USER OF WS-PERM
           MOVE WF-SITE TO CA-SITE OF WS-PERM
           MOVE "AR.RECEIPT.POST" TO PK-PERMISSION
           CALL "PERMCHK" USING WS-PERM
           MOVE "PERMCHK allows SYSADM to post a receipt" TO TK-NAME
           IF CA-RETURN-CODE OF WS-PERM = 0 AND PK-ALLOWED = "Y"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-PERM TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

      *> AUDITOR is the read-only user of CONVENTIONS 4.7.
           INITIALIZE WS-PERM
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERM
           MOVE WF-COMPANY TO CA-COMPANY OF WS-PERM
           MOVE "AUDITOR" TO CA-USER OF WS-PERM
           MOVE "AR.RECEIPT.POST" TO PK-PERMISSION
           CALL "PERMCHK" USING WS-PERM
           MOVE "PERMCHK refuses the read-only auditor with 06"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-PERM = 6 AND PK-ALLOWED = "N"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-PERM TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "PERMCHK explains a refusal instead of staying silent"
               TO TK-NAME
           IF PK-REASON-TEXT NOT = SPACES
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> 5000 - AUDITLOG.  COM-013: who changed what, when and why,
      *> with the authority reference, for every sensitive change.
      *>----------------------------------------------------------------
      *> REQ: COM-013, COM-019
       5000-AUDITLOG-WRITE.
           INITIALIZE WS-AUDT
           MOVE "AUDITLOG" TO CA-PROGRAM OF WS-AUDT
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUDT
           MOVE WF-COMPANY TO CA-COMPANY OF WS-AUDT
           MOVE WF-USER TO CA-USER OF WS-AUDT
           MOVE WF-OPEN-DATE TO CA-BUSINESS-DATE OF WS-AUDT
           MOVE "core.CreditLimit" TO AU-ENTITY-TYPE
           MOVE "STC/ZZCUST0001" TO AU-ENTITY-KEY
           MOVE "AMEND" TO AU-ACTION
           MOVE "CreditLimit" TO AU-FIELD-NAME
           MOVE "5000.00" TO AU-OLD-VALUE
           MOVE "7500.00" TO AU-NEW-VALUE
           MOVE "Annual review of the credit limit" TO AU-REASON
           MOVE "APPR-2026-000123" TO AU-AUTHORITY-REF
           MOVE "TDBSVC" TO AU-SOURCE
           MOVE "Y" TO AU-SENSITIVE
           CALL "AUDITLOG" USING WS-AUDT
           MOVE "AUDITLOG WRITE records a sensitive amendment"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-AUDT = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-AUDT TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "AUDITLOG returns the identity of the row it wrote"
               TO TK-NAME
           IF AU-AUDIT-ID > 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

      *> Read it back on the same connection and transaction.
           MOVE "SNGL" TO SQLIO-FUNC
           MOVE 1 TO SQLIO-CURSOR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(10)) "
                  "FROM core.AuditLog WHERE EntityKey = ? "
                  "AND Action = ? AND OldValue = ? AND NewValue = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE "STC/ZZCUST0001" TO SQLIO-PARM-VAL(1)
           MOVE "AMEND" TO SQLIO-PARM-VAL(2)
           MOVE "5000.00" TO SQLIO-PARM-VAL(3)
           MOVE "7500.00" TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           MOVE "AUDITLOG keeps the old and the new value" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE FUNCTION NUMVAL(WS-PROBE-COL) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 6000 - EXCPRAIS.  COM-017 holds, warnings and errors must be
      *> distinguishable - the service maps the severity onto the
      *> return code (HOLD 06, WARNING 04, ERROR 00 with the exception
      *> recorded).  COM-022 an exception is owned by somebody.
      *>----------------------------------------------------------------
      *> REQ: COM-017, COM-022
       6000-EXCPRAIS-RAISE.
           INITIALIZE WS-EXCP
           MOVE "EXCPRAIS" TO CA-PROGRAM OF WS-EXCP
           MOVE "RAISE" TO CA-FUNCTION OF WS-EXCP
           MOVE WF-COMPANY TO CA-COMPANY OF WS-EXCP
           MOVE WF-USER TO CA-USER OF WS-EXCP
           MOVE WF-OPEN-DATE TO CA-BUSINESS-DATE OF WS-EXCP
           MOVE "AR" TO EX-MODULE
           MOVE "HOLD" TO EX-SEVERITY
           MOVE "CREDIT-LIMIT" TO EX-CATEGORY
           MOVE "Credit limit exceeded by 1,250.00 GBP"
               TO EX-MESSAGE-TEXT
           MOVE "SORDER" TO EX-DOCUMENT-TYPE
           MOVE "ZZSO-2026-004417" TO EX-DOCUMENT-REF
           MOVE "CREDMGR" TO EX-OWNER-USER
           MOVE "CREDIT" TO EX-OWNER-ROLE
           MOVE 1 TO EX-PRIORITY
           MOVE 1250.00 TO EX-AMOUNT-GBP
           CALL "EXCPRAIS" USING WS-EXCP
           MOVE "EXCPRAIS reports a HOLD as return code 06 (COM-017)"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-EXCP = 6
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-EXCP TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "EXCPRAIS records the exception and returns its id"
               TO TK-NAME
           IF EX-EXCEPTION-ID > 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           MOVE "EXCPRAIS opens the exception for the work list"
               TO TK-NAME
           MOVE "OPEN" TO TK-EXPECTED-X
           MOVE EX-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *> A warning is a lesser thing than a hold and says so.
           INITIALIZE WS-EXCP
           MOVE "RAISE" TO CA-FUNCTION OF WS-EXCP
           MOVE WF-COMPANY TO CA-COMPANY OF WS-EXCP
           MOVE WF-USER TO CA-USER OF WS-EXCP
           MOVE "AP" TO EX-MODULE
           MOVE "WARNING" TO EX-SEVERITY
           MOVE "PRICE-VARIANCE" TO EX-CATEGORY
           MOVE "Invoice price differs from the order price"
               TO EX-MESSAGE-TEXT
           MOVE "APMGR" TO EX-OWNER-ROLE
           CALL "EXCPRAIS" USING WS-EXCP
           MOVE "EXCPRAIS reports a WARNING as return code 04"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-EXCP = 4
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-EXCP TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> 7000 - IDEMPOT.  COM-015: repeating a confirmed instruction
      *> must not create a second financial effect, and the user must
      *> be able to tell an existing success from new work and from an
      *> unresolved outcome - STARTED / INPROGRESS / COMPLETED.
      *>----------------------------------------------------------------
      *> REQ: COM-015, COM-014
       7000-IDEMPOT-CYCLE.
           MOVE "TDBSVC-IDEMPOT-000000000000000000A1" TO WS-CORR-A
           MOVE "TDBSVC-IDEMPOT-000000000000000000B2" TO WS-CORR-B

           INITIALIZE WS-IDEM
           MOVE "IDEMPOT" TO CA-PROGRAM OF WS-IDEM
           MOVE "BEGIN" TO CA-FUNCTION OF WS-IDEM
           MOVE WF-COMPANY TO CA-COMPANY OF WS-IDEM
           MOVE WF-USER TO CA-USER OF WS-IDEM
           MOVE WS-CORR-A TO CA-CORRELATION-ID OF WS-IDEM
           MOVE "AR.RECEIPT.POST" TO ID-OPERATION
           CALL "IDEMPOT" USING WS-IDEM
           MOVE "IDEMPOT BEGIN lets new work proceed" TO TK-NAME
           IF CA-RETURN-CODE OF WS-IDEM = 0 AND ID-STARTED
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-IDEM TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

      *> Second attempt while the first is unresolved: 06, INPROGRESS.
           MOVE "BEGIN" TO CA-FUNCTION OF WS-IDEM
           MOVE "AR.RECEIPT.POST" TO ID-OPERATION
           CALL "IDEMPOT" USING WS-IDEM
           MOVE "IDEMPOT reports an attempt already in progress"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-IDEM = 6 AND ID-INPROGRESS
               PERFORM TK-PASS
           ELSE
               MOVE ID-OUTCOME TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "END" TO CA-FUNCTION OF WS-IDEM
           MOVE "COMPLETED" TO ID-STATUS
           MOVE "ZZRCPT-2026-000911" TO ID-RESULT-REF
           MOVE "Receipt posted" TO ID-RESULT-MESSAGE
           CALL "IDEMPOT" USING WS-IDEM
           MOVE "IDEMPOT END closes the instruction as COMPLETED"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-IDEM = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-IDEM TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

      *> Repeat of a completed instruction: 04 plus the earlier result.
           MOVE "BEGIN" TO CA-FUNCTION OF WS-IDEM
           MOVE "AR.RECEIPT.POST" TO ID-OPERATION
           MOVE SPACES TO ID-RESULT-REF
           CALL "IDEMPOT" USING WS-IDEM
           MOVE "IDEMPOT replays the first result, posting only once"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-IDEM = 4 AND ID-COMPLETED
               PERFORM TK-PASS
           ELSE
               MOVE ID-OUTCOME TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "IDEMPOT hands back the original result reference"
               TO TK-NAME
           MOVE "ZZRCPT-2026-000911" TO TK-EXPECTED-X
           MOVE ID-RESULT-REF TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *> A different correlation id is different work.
           MOVE "BEGIN" TO CA-FUNCTION OF WS-IDEM
           MOVE WS-CORR-B TO CA-CORRELATION-ID OF WS-IDEM
           MOVE "AR.RECEIPT.POST" TO ID-OPERATION
           CALL "IDEMPOT" USING WS-IDEM
           MOVE "IDEMPOT treats a new correlation id as new work"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-IDEM = 0 AND ID-STARTED
               PERFORM TK-PASS
           ELSE
               MOVE ID-OUTCOME TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> 8000 - JOBLOG.  COM-018 / COM-014: every batch run registers
      *> itself so that an abandoned run is visible as one still marked
      *> RUNNING rather than silently missing.
      *>----------------------------------------------------------------
      *> REQ: COM-014, COM-018
       8000-JOBLOG-CYCLE.
           INITIALIZE WS-JOBL
           MOVE "JOBLOG" TO CA-PROGRAM OF WS-JOBL
           MOVE "START" TO CA-FUNCTION OF WS-JOBL
           MOVE WF-COMPANY TO CA-COMPANY OF WS-JOBL
           MOVE WF-SITE TO CA-SITE OF WS-JOBL
           MOVE WF-USER TO CA-USER OF WS-JOBL
           MOVE WF-OPEN-DATE TO CA-BUSINESS-DATE OF WS-JOBL
           MOVE "ZZTDBSVC" TO JL-JOB-NAME
           MOVE "CO=STC ASAT=20261015" TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBL
           MOVE JL-JOB-RUN-ID TO WS-JOB-RUN-ID
           MOVE "JOBLOG START registers a run and returns its id"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-JOBL = 0 AND JL-JOB-RUN-ID > 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-JOBL TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "JOBLOG START marks the run as RUNNING" TO TK-NAME
           MOVE "RUNNING" TO TK-EXPECTED-X
           MOVE JL-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "END" TO CA-FUNCTION OF WS-JOBL
           MOVE "COMPLETE" TO JL-STATUS
           MOVE 0 TO JL-RETURN-CODE
           MOVE 417 TO JL-ROWS-PROCESSED
           MOVE "run\spool\ZZTDBSVC-STC-20261015120000.lst"
               TO JL-SPOOL-FILE
           MOVE "417 rows reported" TO JL-MESSAGE-TEXT
           CALL "JOBLOG" USING WS-JOBL
           MOVE "JOBLOG END closes the run with its spool file"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-JOBL = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-JOBL TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "JOBLOG hands COMPLETE back in the caller's wording"
               TO TK-NAME
           MOVE "COMPLETE" TO TK-EXPECTED-X
           MOVE JL-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *> core.JobRun stores its own vocabulary: COMPLETE is held as OK.
           MOVE "SNGL" TO SQLIO-FUNC
           MOVE 1 TO SQLIO-CURSOR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Status FROM core.JobRun WHERE JobRunId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-JOB-RUN-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           MOVE "JOBLOG stores the run as OK in core.JobRun" TO TK-NAME
           MOVE "OK" TO TK-EXPECTED-X
           MOVE WS-PROBE-COL TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> 9100 - PHASE B.  Input that the service must refuse on its own,
      *> without reaching the database.  An online program must never
      *> crash on bad input: it validates and returns 08 (CONVENTIONS
      *> 3.3), because a COBOL runtime error would take the hosting
      *> .NET process down with it.
      *>----------------------------------------------------------------
      *> REQ: COM-017
       9100-INPUT-REFUSALS.
           INITIALIZE WS-PARM
           MOVE "GET" TO CA-FUNCTION OF WS-PARM
           MOVE WF-COMPANY TO CA-COMPANY OF WS-PARM
           MOVE SPACES TO PM-PARAM-CODE
           CALL "PARMGET" USING WS-PARM
           MOVE "PARMGET rejects a blank parameter code" TO TK-NAME
           IF CA-RETURN-CODE OF WS-PARM = 8
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           INITIALIZE WS-NUMG
           MOVE "NEXT" TO CA-FUNCTION OF WS-NUMG
           MOVE WF-COMPANY TO CA-COMPANY OF WS-NUMG
           MOVE SPACES TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUMG
           MOVE "NUMGEN rejects a blank series code" TO TK-NAME
           IF CA-RETURN-CODE OF WS-NUMG = 8
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           INITIALIZE WS-PERC
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERC
           MOVE WF-COMPANY TO CA-COMPANY OF WS-PERC
           MOVE 20260230 TO PC-POSTING-DATE
           MOVE "POST" TO PC-ACTION
           CALL "PERIODCK" USING WS-PERC
           MOVE "PERIODCK rejects 30 February before touching SQL"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-PERC = 8 AND PC-ALLOWED = "N"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           INITIALIZE WS-PERM
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERM
           MOVE WF-COMPANY TO CA-COMPANY OF WS-PERM
           MOVE WF-USER TO CA-USER OF WS-PERM
           MOVE SPACES TO PK-PERMISSION
           CALL "PERMCHK" USING WS-PERM
           MOVE "PERMCHK rejects a blank permission code" TO TK-NAME
           IF CA-RETURN-CODE OF WS-PERM = 8
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           INITIALIZE WS-AUDT
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUDT
           MOVE WF-COMPANY TO CA-COMPANY OF WS-AUDT
           MOVE WF-USER TO CA-USER OF WS-AUDT
           MOVE SPACES TO AU-ENTITY-TYPE
           MOVE "AMEND" TO AU-ACTION
           CALL "AUDITLOG" USING WS-AUDT
           MOVE "AUDITLOG refuses a write with no entity type"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-AUDT = 8
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           INITIALIZE WS-AUDT
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUDT
           MOVE WF-COMPANY TO CA-COMPANY OF WS-AUDT
           MOVE SPACES TO CA-USER OF WS-AUDT
           MOVE "core.CreditLimit" TO AU-ENTITY-TYPE
           MOVE "STC/ZZCUST0001" TO AU-ENTITY-KEY
           MOVE "AMEND" TO AU-ACTION
           CALL "AUDITLOG" USING WS-AUDT
           MOVE "AUDITLOG refuses an entry with no user" TO TK-NAME
           IF CA-RETURN-CODE OF WS-AUDT = 8
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           INITIALIZE WS-EXCP
           MOVE "RAISE" TO CA-FUNCTION OF WS-EXCP
           MOVE WF-COMPANY TO CA-COMPANY OF WS-EXCP
           MOVE WF-USER TO CA-USER OF WS-EXCP
           MOVE "AP" TO EX-MODULE
           MOVE "SHOUTING" TO EX-SEVERITY
           MOVE "TEST" TO EX-CATEGORY
           MOVE "Severity that does not exist" TO EX-MESSAGE-TEXT
           MOVE "APMGR" TO EX-OWNER-ROLE
           CALL "EXCPRAIS" USING WS-EXCP
           MOVE "EXCPRAIS rejects a severity outside HOLD/WARNING/ERROR"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-EXCP = 8
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           INITIALIZE WS-EXCP
           MOVE "RAISE" TO CA-FUNCTION OF WS-EXCP
           MOVE WF-COMPANY TO CA-COMPANY OF WS-EXCP
           MOVE WF-USER TO CA-USER OF WS-EXCP
           MOVE "AR" TO EX-MODULE
           MOVE "ERROR" TO EX-SEVERITY
           MOVE "TEST" TO EX-CATEGORY
           MOVE SPACES TO EX-MESSAGE-TEXT
           MOVE "CREDMGR" TO EX-OWNER-ROLE
           CALL "EXCPRAIS" USING WS-EXCP
           MOVE "EXCPRAIS refuses an exception with no message"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-EXCP = 8
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           INITIALIZE WS-EXCP
           MOVE "RAISE" TO CA-FUNCTION OF WS-EXCP
           MOVE WF-COMPANY TO CA-COMPANY OF WS-EXCP
           MOVE WF-USER TO CA-USER OF WS-EXCP
           MOVE "AR" TO EX-MODULE
           MOVE "ERROR" TO EX-SEVERITY
           MOVE "TEST" TO EX-CATEGORY
           MOVE "An exception that nobody owns" TO EX-MESSAGE-TEXT
           MOVE SPACES TO EX-OWNER-USER
           MOVE SPACES TO EX-OWNER-ROLE
           CALL "EXCPRAIS" USING WS-EXCP
           MOVE "EXCPRAIS refuses an exception with no owner (COM-022)"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-EXCP = 8
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           INITIALIZE WS-IDEM
           MOVE "BEGIN" TO CA-FUNCTION OF WS-IDEM
           MOVE WF-COMPANY TO CA-COMPANY OF WS-IDEM
           MOVE SPACES TO CA-CORRELATION-ID OF WS-IDEM
           MOVE "AR.RECEIPT.POST" TO ID-OPERATION
           CALL "IDEMPOT" USING WS-IDEM
           MOVE "IDEMPOT rejects a blank correlation id" TO TK-NAME
           IF CA-RETURN-CODE OF WS-IDEM = 8
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           INITIALIZE WS-JOBL
           MOVE "START" TO CA-FUNCTION OF WS-JOBL
           MOVE WF-COMPANY TO CA-COMPANY OF WS-JOBL
           MOVE WF-USER TO CA-USER OF WS-JOBL
           MOVE SPACES TO JL-JOB-NAME
           CALL "JOBLOG" USING WS-JOBL
           MOVE "JOBLOG rejects a run with no job name" TO TK-NAME
           IF CA-RETURN-CODE OF WS-JOBL = 8
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           INITIALIZE WS-JOBL
           MOVE "END" TO CA-FUNCTION OF WS-JOBL
           MOVE WF-COMPANY TO CA-COMPANY OF WS-JOBL
           MOVE 0 TO JL-JOB-RUN-ID
           MOVE "COMPLETE" TO JL-STATUS
           CALL "JOBLOG" USING WS-JOBL
           MOVE "JOBLOG END rejects a run id that was never started"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-JOBL = 8
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           INITIALIZE WS-JOBL
           MOVE "END" TO CA-FUNCTION OF WS-JOBL
           MOVE WF-COMPANY TO CA-COMPANY OF WS-JOBL
           MOVE 1 TO JL-JOB-RUN-ID
           MOVE "FINISHED" TO JL-STATUS
           CALL "JOBLOG" USING WS-JOBL
           MOVE "JOBLOG rejects an end status it does not know"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-JOBL = 8
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> 9200 - PHASE B.  Refusals that the database decides.  Several
      *> core procedures report these with THROW under SET XACT_ABORT
      *> ON, which also dooms the caller's transaction - that is why
      *> they are kept out of the fixture transaction above.
      *>----------------------------------------------------------------
      *> REQ: COM-005, COM-009, COM-025
       9200-BUSINESS-REFUSALS.
      *> COM-025 - a tuning parameter that is not held must never stop
      *> a business action: with a default the answer is 04, without
      *> one it is a plain 16, and neither is a severe error.
           INITIALIZE WS-PARM
           MOVE "GET" TO CA-FUNCTION OF WS-PARM
           MOVE WF-COMPANY TO CA-COMPANY OF WS-PARM
           MOVE WF-OPEN-DATE TO CA-BUSINESS-DATE OF WS-PARM
           MOVE "STERP.TEST.NOTHELD" TO PM-PARAM-CODE
           MOVE "Y" TO PM-USE-DEFAULT
           MOVE "14" TO PM-DEFAULT-TEXT
           CALL "PARMGET" USING WS-PARM
           MOVE "PARMGET falls back to the caller default with 04"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-PARM = 4
              AND CA-REASON-CODE OF WS-PARM = "PRM0004"
              AND PM-VALUE-TEXT = "14"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-PARM TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           INITIALIZE WS-PARM
           MOVE "GET" TO CA-FUNCTION OF WS-PARM
           MOVE WF-COMPANY TO CA-COMPANY OF WS-PARM
           MOVE WF-OPEN-DATE TO CA-BUSINESS-DATE OF WS-PARM
           MOVE "STERP.TEST.NOTHELD" TO PM-PARAM-CODE
           MOVE "N" TO PM-USE-DEFAULT
           CALL "PARMGET" USING WS-PARM
           MOVE "PARMGET reports 16 when nothing is held" TO TK-NAME
           IF CA-RETURN-CODE OF WS-PARM = 16 AND PM-FOUND = "N"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-PARM TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           INITIALIZE WS-NUMG
           MOVE "NEXT" TO CA-FUNCTION OF WS-NUMG
           MOVE WF-COMPANY TO CA-COMPANY OF WS-NUMG
           MOVE WF-USER TO CA-USER OF WS-NUMG
           MOVE "ZZNOSUCH" TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUMG
           MOVE "NUMGEN reports an unknown series rather than guessing"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-NUMG NOT < 8
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           INITIALIZE WS-PERC
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERC
           MOVE WF-COMPANY TO CA-COMPANY OF WS-PERC
           MOVE WF-USER TO CA-USER OF WS-PERC
           MOVE 19900101 TO PC-POSTING-DATE
           MOVE "POST" TO PC-ACTION
           CALL "PERIODCK" USING WS-PERC
           MOVE "PERIODCK refuses a date with no accounting period"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-PERC NOT < 8 AND PC-ALLOWED = "N"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

       COPY TESTKITP.
