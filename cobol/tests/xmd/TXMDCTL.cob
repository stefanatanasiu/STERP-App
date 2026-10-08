      *>================================================================
      *> PROGRAM-ID : TXMDCTL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : CROSS MODULE CONTROLS (XMD) UNIT TESTS
      *> PURPOSE    : CHAINTRC INPUT CONTROL AND CHAIN ASSEMBLY, AND
      *>              THE PUBLISHED SHARED MEASURE DEFINITIONS THAT
      *>              THE CONTROL REPORTS QUOTE.  A BAD REQUEST COMES
      *>              BACK AS 08 AND A REFERENCE THAT DOES NOT EXIST
      *>              AS 16 - NEITHER EVER CRASHES THE ENGINE.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: XMD-001, XMD-002, XMD-003, XMD-004, XMD-010, XMD-012,
      *> REQ: COM-017, D-19
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXMDCTL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY CHAINTRCCA REPLACING ==DFHCOMMAREA== BY ==WS-CT-CA==.
       01  WS-ROW.
           05  WS-COL OCCURS 10 TIMES  PIC X(120).
       01  WS-WORK.
           05  WS-DB-UP                PIC X VALUE "N".
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-N                    PIC S9(7) VALUE 0.
           05  WS-ORDER                PIC X(40) VALUE SPACES.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TXMDCTL chain trace and measures" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-INPUT-CONTROL
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               MOVE "chain trace (no database)" TO TK-NAME
               PERFORM TK-SKIP
           ELSE
               PERFORM 2000-MEASURES
               PERFORM 3000-TRACE-AN-ORDER
           END-IF
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
      *> REQ: COM-017  holds, warnings and errors stay distinguishable
      *> and a bad request never terminates the host process
      *>----------------------------------------------------------------
       1000-INPUT-CONTROL.
           INITIALIZE WS-CT-CA
           MOVE "TRACE" TO CA-FUNCTION OF WS-CT-CA
           MOVE SPACES TO CA-COMPANY OF WS-CT-CA
           MOVE "ORDER" TO CT-DOC-TYPE
           MOVE "ANY" TO CT-DOC-REF
           CALL "CHAINTRC" USING WS-CT-CA
           MOVE "a trace without a company is refused with 08"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CT-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           INITIALIZE WS-CT-CA
           MOVE "TRACE" TO CA-FUNCTION OF WS-CT-CA
           MOVE "STC" TO CA-COMPANY OF WS-CT-CA
           MOVE "ORDER" TO CT-DOC-TYPE
           MOVE SPACES TO CT-DOC-REF
           CALL "CHAINTRC" USING WS-CT-CA
           MOVE "a trace without a reference is refused with 08"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CT-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           INITIALIZE WS-CT-CA
           MOVE "TRACE" TO CA-FUNCTION OF WS-CT-CA
           MOVE "STC" TO CA-COMPANY OF WS-CT-CA
           MOVE "LEDGER" TO CT-DOC-TYPE
           MOVE "X" TO CT-DOC-REF
           CALL "CHAINTRC" USING WS-CT-CA
           MOVE "an unsupported document type is refused with 08"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CT-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the refusal carries a stable reason code"
               TO TK-NAME
           MOVE "XMD0003" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CT-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-CT-CA
           MOVE "LIST" TO CA-FUNCTION OF WS-CT-CA
           MOVE "STC" TO CA-COMPANY OF WS-CT-CA
           MOVE "ORDER" TO CT-DOC-TYPE
           MOVE "ANY" TO CT-DOC-REF
           CALL "CHAINTRC" USING WS-CT-CA
           MOVE "an unknown function is refused, not guessed at"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CT-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       0100-PROBE-DATABASE.
           MOVE "N" TO WS-DB-UP
           MOVE SPACES TO SQLIO-STMT
           MOVE "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                TO SQLIO-STMT(1:44)
           MOVE "core.Company" TO SQLIO-STMT(45:12)
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-OK
               MOVE "Y" TO WS-DB-UP
           END-IF.

      *>----------------------------------------------------------------
      *> REQ: XMD-010, D-19  one published definition per measure, with
      *> an explicit rule for a zero denominator
      *>----------------------------------------------------------------
       2000-MEASURES.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                  "rpt.MetricDefinition WHERE MetricCode IN ("
                  "'OTIF.CUSTOMER','OTIF.SUPPLIER') AND IsActive = "
                  "1 AND ZeroDenominatorRule LIKE 'Undefined%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8100-ONE-COUNT
           MOVE "both OTIF measures are published once each"
               TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           MOVE WS-N TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(DecisionRef,'') FROM "
                  "rpt.MetricDefinition WHERE MetricCode = "
                  "'OTIF.SUPPLIER'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "supplier OTIF cites the decision that defines it"
               TO TK-NAME
           MOVE "D-19" TO TK-EXPECTED-X
           MOVE WS-COL(1)(1:4) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(DISTINCT "
                  "ReportCode)) FROM core.ReportDef WHERE "
                  "ProgramName IN ('RECONCTL','EXCPAGE') AND "
                  "IsActive = 1"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8100-ONE-COUNT
           MOVE "the control reports are in the report catalogue"
               TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           MOVE WS-N TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> REQ: XMD-001, XMD-012  a real order gives an ordered chain;
      *> a reference that does not exist gives 16, not an empty chain
      *> pretending to be complete
      *>----------------------------------------------------------------
       3000-TRACE-AN-ORDER.
           INITIALIZE WS-CT-CA
           MOVE "TRACE" TO CA-FUNCTION OF WS-CT-CA
           MOVE "STC" TO CA-COMPANY OF WS-CT-CA
           MOVE "ORDER" TO CT-DOC-TYPE
           MOVE "ZZ-NO-SUCH-ORDER" TO CT-DOC-REF
           CALL "CHAINTRC" USING WS-CT-CA
           MOVE "an unknown order reference answers 16" TO TK-NAME
           MOVE 16 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CT-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "an unknown reference is reported as broken"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE CT-BROKEN TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 s.OrderNumber FROM "
                  "so.DeliverySchedule s WHERE s.CompanyCode = "
                  "'STC' ORDER BY s.OrderNumber"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF NOT SQLIO-OK
               MOVE "chain trace of a live order (no orders)"
                   TO TK-NAME
               PERFORM TK-SKIP
               GO TO 3000-EXIT
           END-IF
           MOVE WS-COL(1) TO WS-ORDER
           INITIALIZE WS-CT-CA
           MOVE "TRACE" TO CA-FUNCTION OF WS-CT-CA
           MOVE "STC" TO CA-COMPANY OF WS-CT-CA
           MOVE "ORDER" TO CT-DOC-TYPE
           MOVE WS-ORDER TO CT-DOC-REF
           CALL "CHAINTRC" USING WS-CT-CA
           MOVE "a live order yields at least one chain step"
               TO TK-NAME
           IF CT-STEP-CNT > 0
               MOVE 1 TO TK-EXPECTED-N
           ELSE
               MOVE 0 TO TK-EXPECTED-N
           END-IF
           MOVE 1 TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the first step is the accepted order schedule"
               TO TK-NAME
           MOVE "Sales order schedules" TO TK-EXPECTED-X
           MOVE CT-STAGE(1) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the chain is named order to cash" TO TK-NAME
           MOVE "O2C" TO TK-EXPECTED-X
           MOVE CT-CHAIN-TYPE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       3000-EXIT.
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
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
               MOVE 120 TO SQLIO-COL-WIDTH(WS-I)
           END-PERFORM.

       COPY TESTKITP.
