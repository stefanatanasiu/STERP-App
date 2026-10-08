      *>================================================================
      *> PROGRAM-ID : TSOEXPEV
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT UNIT TESTS
      *> PURPOSE    : EXPEVID - EXPORT EVIDENCE, THE THREE MONTH PROOF
      *>              OF EXPORT DEADLINE AND THE VAT THAT FALLS DUE
      *>              WHEN IT IS MISSED, THE COMMODITY CODE BLOCK ON
      *>              THE CUSTOMS HAND OFF, AND THE GB TO NORTHERN
      *>              IRELAND RULE OF D-23.
      *>              THE FIXTURE DESPATCH IS WRITTEN INSIDE
      *>              BEGN ... ROLB SO NOTHING SURVIVES THE RUN.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: SO-025, SO-026   AC: AC-129
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSOEXPEV.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY EXPEVIDCA REPLACING ==DFHCOMMAREA== BY ==WS-EXP-CA==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "Y".
           05  WS-IN-TXN               PIC X VALUE "N".
           05  WS-FOUND                PIC X VALUE "N".
       01  WS-PROBE.
           05  WS-P-CUSTOMER           PIC X(20).
           05  WS-P-ITEM               PIC X(40).
       01  WS-ONE-COL.
           05  WS-C1                   PIC X(40).
           05  WS-C2                   PIC X(40).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-N                    PIC S9(15)V9(6) VALUE 0.
           05  WS-DESPATCH-ID          PIC S9(17) VALUE 0.
           05  WS-INT-ED               PIC -(8)9.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TSOEXPEV export evidence and VAT" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-NO-DATABASE-NEEDED
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-DATABASE-TESTS
           ELSE
               PERFORM 0150-CLEAN-UP
               PERFORM 0200-BEGIN
               PERFORM 0400-MAKE-EXPORT-FIXTURE
               IF WS-DB-UP = "Y"
                   PERFORM 2000-DOMESTIC-CODE-REFUSED
                   PERFORM 2100-NO-COMMODITY-CODE
                   PERFORM 2200-DEADLINE-IS-THREE-MONTHS
                   PERFORM 2300-OVERDUE-RAISES-THE-VAT
                   PERFORM 2400-GB-TO-NI-IS-DOMESTIC
                   PERFORM 2500-PROOF-OF-EXPORT-RECORDED
               ELSE
                   PERFORM 0900-SKIP-DATABASE-TESTS
               END-IF
               PERFORM 0300-ROLLBACK
               PERFORM 0150-CLEAN-UP
           END-IF
           PERFORM TK-END
           STOP RUN.
      *>----------------------------------------------------------------
       0100-PROBE-DATABASE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) c.CustomerCode, ISNULL((SELECT "
                  "TOP (1) i.ItemCode FROM inv.Item i WHERE "
                  "i.CompanyCode = 'STC' ORDER BY i.ItemCode),'') "
                  "FROM ar.Customer c WHERE c.CompanyCode = 'STC' "
                  "ORDER BY c.CustomerCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "N" TO WS-DB-UP
           END-IF
           IF WS-P-ITEM = SPACES
               MOVE "N" TO WS-DB-UP
           END-IF.

       0200-BEGIN.
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR
               MOVE "N" TO WS-DB-UP
           ELSE
               MOVE "Y" TO WS-IN-TXN
           END-IF.

       0300-ROLLBACK.
           IF WS-IN-TXN = "Y"
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-PROBE
               MOVE "N" TO WS-IN-TXN
           END-IF.

      *>   EXPEVID commits its own unit of work, and SQLIO transaction
      *>   control is per connection, so the rollback alone cannot be
      *>   relied on.  The fixture is removed by name before and after.
       0150-CLEAN-UP.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; DELETE so.DespatchLine WHERE "
                  "DespatchId IN (SELECT DespatchId FROM "
                  "so.Despatch WHERE CompanyCode = 'STC' AND "
                  "DespatchRef = 'ZZSO-DN-EXP1'); DELETE "
                  "so.Despatch WHERE CompanyCode = 'STC' AND "
                  "DespatchRef = 'ZZSO-DN-EXP1'; DELETE "
                  "core.ExceptionItem WHERE CompanyCode = 'STC' AND "
                  "DocumentRef = 'ZZSO-DN-EXP1'; DELETE "
                  "core.AuditLog WHERE CorrelationId LIKE "
                  "'ZZSOTEST%'; DELETE core.IdempotencyKey WHERE "
                  "CorrelationId LIKE 'ZZSOTEST%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           MOVE 7 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

       0900-SKIP-DATABASE-TESTS.
           MOVE "a domestic VAT code is refused on an export"
               TO TK-NAME
           MOVE "no STERP database or no seeded data" TO TK-DETAIL
           PERFORM TK-SKIP
           MOVE "no commodity code blocks the customs hand-off"
               TO TK-NAME
           MOVE "no STERP database or no seeded data" TO TK-DETAIL
           PERFORM TK-SKIP
           MOVE "the proof deadline is three months from supply"
               TO TK-NAME
           MOVE "no STERP database or no seeded data" TO TK-DETAIL
           PERFORM TK-SKIP
           MOVE "three months with no proof becomes OVERDUE"
               TO TK-NAME
           MOVE "no STERP database or no seeded data" TO TK-DETAIL
           PERFORM TK-SKIP
           MOVE "a GB to NI movement is a UK domestic supply"
               TO TK-NAME
           MOVE "no STERP database or no seeded data" TO TK-DETAIL
           PERFORM TK-SKIP.
      *>----------------------------------------------------------------
       1000-NO-DATABASE-NEEDED.
           PERFORM 1100-BUILD-EXPORT
           MOVE "WIBBLE" TO CA-FUNCTION OF WS-EXP-CA
           CALL "EXPEVID" USING WS-EXP-CA
           MOVE "an unknown function is refused, not obeyed"
               TO TK-NAME
           MOVE "EXP0009" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-EXP-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 1100-BUILD-EXPORT
           MOVE SPACES TO XE-DESPATCH-REF
           CALL "EXPEVID" USING WS-EXP-CA
           MOVE "a despatch reference is required" TO TK-NAME
           MOVE "EXP0001" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-EXP-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
      *>----------------------------------------------------------------
       1100-BUILD-EXPORT.
           INITIALIZE WS-EXP-CA
           MOVE "CAPTURE" TO CA-FUNCTION OF WS-EXP-CA
           MOVE "STC" TO CA-COMPANY OF WS-EXP-CA
           MOVE "RDG" TO CA-SITE OF WS-EXP-CA
           MOVE "VATOWNER" TO CA-USER OF WS-EXP-CA
           MOVE 20261016 TO CA-BUSINESS-DATE OF WS-EXP-CA
           MOVE "ZZSOTEST-EXPEVID-0001" TO
               CA-CORRELATION-ID OF WS-EXP-CA
           MOVE "ZZSO-DN-EXP1" TO XE-DESPATCH-REF
           MOVE "US" TO XE-DEST-COUNTRY
           MOVE "CIP" TO XE-INCOTERMS-RULE
           MOVE "Port of Felixstowe" TO XE-INCOTERMS-PLACE
           MOVE "GB123456789000" TO XE-CUSTOMER-EORI
           MOVE "MRN26GB0001" TO XE-DECLARATION-REF
           MOVE "EXP" TO XE-VAT-CODE
           MOVE 20261016 TO XE-AS-AT-DATE.
      *>----------------------------------------------------------------
      *> 0400  ONE CONFIRMED EXPORT DESPATCH OF 10 UNITS WITH A
      *>       CUSTOMS VALUE OF £1,000.00 AND NO COMMODITY CODE.
      *>----------------------------------------------------------------
       0400-MAKE-EXPORT-FIXTURE.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT so.Despatch (CompanyCode, SiteCode, "
                  "DespatchRef, CustomerCode, OrderNumber, "
                  "DespatchDate, PostingDate, Status, IsExport, "
                  "DestinationCountry, ExportEvidenceStatus, "
                  "CorrelationId, CreatedBy) VALUES ('STC', 'RDG', "
                  "?, ?, 'ZZSO-ORD-EXP1', '20260715', '20260715', "
                  "'CONFIRMED', 1, 'US', 'PENDING', "
                  "'ZZSOTEST-EXPEVID', 'VATOWNER'); SELECT "
                  "CAST(SCOPE_IDENTITY() AS varchar(24)), ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE "ZZSO-DN-EXP1" TO SQLIO-PARM-VAL(1)
           MOVE WS-P-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "N" TO WS-DB-UP
               GO TO 0400-EXIT
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WS-C1)
           MOVE WS-N TO WS-DESPATCH-ID
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT so.DespatchLine (DespatchId, LineNum, "
                  "ItemCode, Description, DespatchedQty, UomCode, "
                  "ConversionFactor, StockQty, CustomsValueGBP, "
                  "CountryOfOrigin) VALUES (?, 1, ?, 'Export test "
                  "line', 10, 'EA', 1, 10, 1000.00, 'GB')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-DESPATCH-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE WS-P-ITEM TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               MOVE "N" TO WS-DB-UP
           END-IF
           MOVE 8 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       0400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 2000  SO-026 - A DESPATCH TO A REST OF WORLD ADDRESS CANNOT
      *>       CARRY A DOMESTIC VAT CODE WITHOUT AUTHORISED EVIDENCE.
      *>----------------------------------------------------------------
       2000-DOMESTIC-CODE-REFUSED.
           PERFORM 1100-BUILD-EXPORT
           MOVE "S" TO XE-VAT-CODE
           CALL "EXPEVID" USING WS-EXP-CA
           MOVE "a domestic VAT code is refused on an export"
               TO TK-NAME
           MOVE "EXP0003" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-EXP-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the United States is rest of world" TO TK-NAME
           MOVE "ROW" TO TK-EXPECTED-X
           MOVE XE-DEST-REGION TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
      *>----------------------------------------------------------------
      *> 2100  SO-026 - A DESPATCH LINE WITH NO COMMODITY CODE BLOCKS
      *>       THE CUSTOMS HAND OFF.
      *>----------------------------------------------------------------
       2100-NO-COMMODITY-CODE.
           PERFORM 1100-BUILD-EXPORT
           CALL "EXPEVID" USING WS-EXP-CA
           MOVE "no commodity code blocks the customs hand-off"
               TO TK-NAME
           MOVE "EXP0007" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-EXP-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the blocked line is named" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE XE-L-BLOCKED(1) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
      *>----------------------------------------------------------------
      *> 2200  SO-026 - THE PERMITTED PERIOD IS THREE MONTHS FROM THE
      *>       SUPPLY: 15 JULY 2026 PLUS THREE MONTHS IS 15 OCTOBER.
      *>----------------------------------------------------------------
       2200-DEADLINE-IS-THREE-MONTHS.
           PERFORM 1100-BUILD-EXPORT
           MOVE 1 TO XE-LINE-CNT
           MOVE 1 TO XE-L-LINE-NUM(1)
           MOVE "85176200" TO XE-L-COMMODITY(1)
           MOVE "GB" TO XE-L-ORIGIN(1)
           MOVE 1000 TO XE-L-CUST-VALUE(1)
           CALL "EXPEVID" USING WS-EXP-CA
           MOVE "the proof deadline is three months from supply"
               TO TK-NAME
           MOVE 20261015 TO TK-EXPECTED-N
           MOVE XE-PROOF-DEADLINE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the evidence is pending until proof arrives"
               TO TK-NAME
           MOVE "PENDING" TO TK-EXPECTED-X
           MOVE XE-EVIDENCE-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the default permitted period is three months"
               TO TK-NAME
           MOVE 3 TO TK-EXPECTED-N
           MOVE XE-PROOF-MONTHS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
      *>----------------------------------------------------------------
      *> 2300  AC-129 - REACHING THREE MONTHS WITH NO PROOF MAKES THE
      *>       EXPORT OVERDUE, RAISES AN EXCEPTION AND REPORTS THE VAT
      *>       THAT NOW FALLS DUE: £1,000.00 AT 20% IS £200.00.
      *>----------------------------------------------------------------
       2300-OVERDUE-RAISES-THE-VAT.
           PERFORM 1100-BUILD-EXPORT
           MOVE "CHECKDUE" TO CA-FUNCTION OF WS-EXP-CA
           MOVE 20261016 TO XE-AS-AT-DATE
           CALL "EXPEVID" USING WS-EXP-CA
           MOVE "N" TO WS-FOUND
           MOVE 0 TO WS-J
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > XE-DUE-CNT
               IF XE-D-DESP-REF(WS-I) = "ZZSO-DN-EXP1"
                   MOVE "Y" TO WS-FOUND
                   MOVE WS-I TO WS-J
               END-IF
           END-PERFORM
           MOVE "three months with no proof becomes OVERDUE"
               TO TK-NAME
           IF WS-FOUND = "Y"
               MOVE "OVERDUE" TO TK-EXPECTED-X
               MOVE XE-D-STATUS(WS-J) TO TK-ACTUAL-X
               PERFORM TK-ASSERT-STR
           ELSE
               MOVE "the overdue export was not listed" TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "the VAT that falls due is reported" TO TK-NAME
           IF WS-FOUND = "Y"
               MOVE 200.00 TO TK-EXPECTED-N
               MOVE XE-D-VAT-DUE(WS-J) TO TK-ACTUAL-N
               PERFORM TK-ASSERT-NUM
           ELSE
               MOVE "the overdue export was not listed" TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "an exception is raised for the VAT owner" TO TK-NAME
           IF XE-EXCEPTION-COUNT > 0
               PERFORM TK-PASS
           ELSE
               MOVE "no exception was raised" TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           PERFORM 1100-BUILD-EXPORT
           MOVE "STATUS" TO CA-FUNCTION OF WS-EXP-CA
           CALL "EXPEVID" USING WS-EXP-CA
           MOVE "the despatch is left marked OVERDUE" TO TK-NAME
           MOVE "OVERDUE" TO TK-EXPECTED-X
           MOVE XE-EVIDENCE-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
      *>----------------------------------------------------------------
      *> 2400  D-23 - GOODS MOVED FROM GREAT BRITAIN TO NORTHERN
      *>       IRELAND ARE UK DOMESTIC SUPPLIES, NOT EXPORTS, SO NO
      *>       PROOF OF EXPORT IS NEEDED.
      *>----------------------------------------------------------------
       2400-GB-TO-NI-IS-DOMESTIC.
           PERFORM 1100-BUILD-EXPORT
           MOVE "XI" TO XE-DEST-COUNTRY
           MOVE "S" TO XE-VAT-CODE
           MOVE 1 TO XE-LINE-CNT
           MOVE 1 TO XE-L-LINE-NUM(1)
           MOVE "85176200" TO XE-L-COMMODITY(1)
           MOVE "GB" TO XE-L-ORIGIN(1)
           MOVE 1000 TO XE-L-CUST-VALUE(1)
           CALL "EXPEVID" USING WS-EXP-CA
           MOVE "a GB to NI movement is a UK domestic supply"
               TO TK-NAME
           MOVE "NI" TO TK-EXPECTED-X
           MOVE XE-DEST-REGION TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a GB to NI movement is not an export" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE XE-IS-EXPORT TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a UK domestic supply needs no proof of export"
               TO TK-NAME
           MOVE "NA" TO TK-EXPECTED-X
           MOVE XE-EVIDENCE-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
      *>----------------------------------------------------------------
      *> 2500  SO-025 - RECORDPF , THE PROOF OF EXPORT ITSELF.  BOTH
      *>       THE TYPE AND THE DATE ARE REQUIRED; PROOF OBTAINED
      *>       AFTER THE THREE MONTH PERIOD DOES NOT RESCUE THE ZERO
      *>       RATE AND THE VAT (£1,000.00 AT 20% = £200.00) STILL
      *>       FALLS DUE; PROOF OBTAINED INSIDE THE PERIOD EVIDENCES
      *>       THE ZERO RATE AND IS WRITTEN TO THE DESPATCH, WHICH
      *>       STATUS THEN READS BACK.  THE DESPATCH IS LEFT OVERDUE
      *>       BY 2300, SO THIS ALSO PROVES THAT LATE EVIDENCE CAN
      *>       STILL CLEAR AN OVERDUE EXPORT.
      *>----------------------------------------------------------------
       2500-PROOF-OF-EXPORT-RECORDED.
           PERFORM 1100-BUILD-EXPORT
           MOVE "RECORDPF" TO CA-FUNCTION OF WS-EXP-CA
           CALL "EXPEVID" USING WS-EXP-CA
           MOVE "proof of export needs both a type and a date"
               TO TK-NAME
           MOVE "EXP0006" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-EXP-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 1100-BUILD-EXPORT
           MOVE "RECORDPF" TO CA-FUNCTION OF WS-EXP-CA
           MOVE "BILL OF LADING" TO XE-PROOF-TYPE
           MOVE 20261101 TO XE-PROOF-DATE
           CALL "EXPEVID" USING WS-EXP-CA
           MOVE "proof obtained after the permitted period fails"
               TO TK-NAME
           MOVE "FAILED" TO TK-EXPECTED-X
           MOVE XE-EVIDENCE-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "late proof is reported as EXP0008" TO TK-NAME
           MOVE "EXP0008" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-EXP-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "late proof leaves the VAT due at 20 per cent"
               TO TK-NAME
           MOVE 200.00 TO TK-EXPECTED-N
           MOVE XE-VAT-DUE-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 1100-BUILD-EXPORT
           MOVE "RECORDPF" TO CA-FUNCTION OF WS-EXP-CA
           MOVE "AIR WAYBILL" TO XE-PROOF-TYPE
           MOVE 20260901 TO XE-PROOF-DATE
           CALL "EXPEVID" USING WS-EXP-CA
           MOVE "proof inside the period evidences the zero rate"
               TO TK-NAME
           MOVE "OBTAINED" TO TK-EXPECTED-X
           MOVE XE-EVIDENCE-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "evidenced export carries no VAT" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE XE-VAT-DUE-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "an evidenced export is no longer overdue" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE XE-IS-OVERDUE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 1100-BUILD-EXPORT
           MOVE "STATUS" TO CA-FUNCTION OF WS-EXP-CA
           CALL "EXPEVID" USING WS-EXP-CA
           MOVE "the proof is written to the despatch itself"
               TO TK-NAME
           MOVE "OBTAINED" TO TK-EXPECTED-X
           MOVE XE-EVIDENCE-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
      *>----------------------------------------------------------------
       COPY TESTKITP.
