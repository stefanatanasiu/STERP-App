      *>================================================================
      *> PROGRAM-ID : TSORETRN
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT UNIT TESTS
      *> PURPOSE    : RETURNS AND SOCREDIT - THE SEPARATION OF
      *>              AUTHORISATION, RECEIPT AND CREDIT, THE D-12
      *>              RETURN WINDOW AND THE VALUE ONLY CONCESSION.
      *>              EVERY DATABASE DEPENDENT ASSERTION RUNS INSIDE
      *>              BEGN ... ROLB SO THE SUITE LEAVES NOTHING
      *>              BEHIND, AND SKIPS WHEN THE DATA IS NOT THERE.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: SO-017, SO-018, SO-019   AC: AC-046
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSORETRN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY RETURNSCA REPLACING ==DFHCOMMAREA== BY ==WS-RET-CA==.
       01  WS-SOCREDIT-CA.
           COPY STCAHDR.
      *>   SOCREDIT declares its commarea in its own LINKAGE SECTION,
      *>   so the part this suite needs is mirrored here field for
      *>   field and the remainder is reserved as filler.
           05  SC-BODY.
               10  T-RETURN-REF        PIC X(30).
               10  T-RETURN-ID         PIC S9(17).
               10  T-CUSTOMER          PIC X(20).
               10  T-SITE              PIC X(10).
               10  T-ORIG-INVOICE      PIC X(30).
               10  T-ORIG-ORDER        PIC X(30).
               10  T-RETURN-STATUS     PIC X(12).
               10  T-CREDIT-NOTE       PIC X(30).
               10  T-CREDIT-TYPE       PIC X(10).
               10  T-REASON-CODE       PIC X(20).
               10  T-REASON            PIC X(200).
               10  T-DOC-DATE          PIC 9(8).
               10  T-TAX-POINT         PIC 9(8).
               10  T-POSTING-DATE      PIC 9(8).
               10  T-CURRENCY          PIC X(3).
               10  T-FX-RATE           PIC S9(9)V9(8) COMP-3.
               10  T-APPROVAL-ID       PIC S9(9) COMP-5.
               10  T-APPROVED-BY       PIC X(20).
               10  T-RETURN-STOCK      PIC X.
               10  T-STOCK-EFFECT      PIC X.
               10  T-ORDER-REOPENED    PIC X.
               10  T-OWN-OBLIGATION    PIC X.
      *>       the money and concession fields have to be mirrored as
      *>       well: INITIALIZE only zeroises named items, and packed
      *>       fields left as part of a FILLER read back as rubbish
               10  T-ORIG-NET-GBP      PIC S9(11)V99 COMP-3.
               10  T-PRIOR-CREDIT-GBP  PIC S9(11)V99 COMP-3.
               10  T-CREDITABLE-REM    PIC S9(11)V99 COMP-3.
               10  T-PRIOR-CRED-QTY    PIC S9(13)V9(6) COMP-3.
               10  T-CREDIT-QTY        PIC S9(13)V9(6) COMP-3.
               10  T-NET               PIC S9(11)V99 COMP-3.
               10  T-VAT               PIC S9(11)V99 COMP-3.
               10  T-GROSS             PIC S9(11)V99 COMP-3.
               10  T-NET-GBP           PIC S9(11)V99 COMP-3.
               10  T-VAT-GBP           PIC S9(11)V99 COMP-3.
               10  T-GROSS-GBP         PIC S9(11)V99 COMP-3.
               10  T-CREDIT-ID         PIC S9(9) COMP-5.
               10  T-OPEN-ITEM-ID      PIC S9(9) COMP-5.
               10  T-JOURNAL-NO        PIC X(30).
               10  T-AUDIT-ID          PIC S9(9) COMP-5.
               10  T-CONCESSION-PCT    PIC S9(3)V9(4) COMP-3.
               10  T-CONCESSION-AMT    PIC S9(11)V99 COMP-3.
               10  T-CONCESSION-VAT    PIC X(4).
               10  T-CONCESSION-RATE   PIC S9(3)V9(4) COMP-3.
               10  FILLER              PIC X(5400).
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "Y".
           05  WS-IN-TXN               PIC X VALUE "N".
       01  WS-PROBE.
           05  WS-P-CUSTOMER           PIC X(20).
           05  WS-P-ITEM               PIC X(40).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-COUNT                PIC S9(4) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TSORETRN customer returns and credits" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-NO-DATABASE-NEEDED
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-DATABASE-TESTS
           ELSE
               PERFORM 0150-CLEAN-UP
               PERFORM 0200-BEGIN
               PERFORM 2000-AUTHORISATION-ALONE
               PERFORM 2100-FORTY-FIVE-DAYS-LATE
               PERFORM 2200-FAULTY-IN-WARRANTY
               PERFORM 2300-FAULTY-OUT-OF-WARRANTY
               PERFORM 0300-ROLLBACK
               PERFORM 0150-CLEAN-UP
           END-IF
           PERFORM 3000-CONCESSION-HAS-NO-STOCK
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
               MOVE "ZZ-TEST-ITEM" TO WS-P-ITEM
           END-IF.

       0200-BEGIN.
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR
               MOVE "N" TO WS-DB-UP
           ELSE
               MOVE "Y" TO WS-IN-TXN
           END-IF.

      *>   RETURNS commits its own unit of work, and SQLIO transaction
      *>   control is per connection, so the rollback alone cannot be
      *>   relied on.  Anything this suite leaves behind is removed by
      *>   its fixture references, before and after the run.
       0150-CLEAN-UP.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; DELETE so.ReturnLine WHERE "
                  "ReturnId IN (SELECT ReturnId FROM "
                  "so.ReturnAuthorisation WHERE CompanyCode = 'STC' "
                  "AND OriginalOrderNumber = 'ZZSO-ORD-0001'); "
                  "DELETE so.ReturnAuthorisation WHERE CompanyCode "
                  "= 'STC' AND OriginalOrderNumber = "
                  "'ZZSO-ORD-0001'; DELETE core.AuditLog WHERE "
                  "CorrelationId LIKE 'ZZSOTEST%'; DELETE "
                  "core.IdempotencyKey WHERE CorrelationId LIKE "
                  "'ZZSOTEST%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           MOVE 7 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

       0300-ROLLBACK.
           IF WS-IN-TXN = "Y"
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-PROBE
               MOVE "N" TO WS-IN-TXN
           END-IF.

       0900-SKIP-DATABASE-TESTS.
           MOVE "a return authorisation moves no stock" TO TK-NAME
           MOVE "no STERP database" TO TK-DETAIL
           PERFORM TK-SKIP
           MOVE "45 days is outside the 30 day window" TO TK-NAME
           MOVE "no STERP database" TO TK-DETAIL
           PERFORM TK-SKIP
           MOVE "faulty goods use the warranty period" TO TK-NAME
           MOVE "no STERP database" TO TK-DETAIL
           PERFORM TK-SKIP
           MOVE "outside warranty needs an approval" TO TK-NAME
           MOVE "no STERP database" TO TK-DETAIL
           PERFORM TK-SKIP.
      *>----------------------------------------------------------------
      *> 1000  THESE FOUR NEED NO DATABASE: THEY NEVER GET PAST THE
      *>       VALIDATION IN RETURNS AND SOCREDIT.
      *>----------------------------------------------------------------
       1000-NO-DATABASE-NEEDED.
           PERFORM 1100-BUILD-RETURN
           MOVE "WIBBLE" TO CA-FUNCTION OF WS-RET-CA
           CALL "RETURNS" USING WS-RET-CA
           MOVE "an unknown function is refused, not obeyed"
               TO TK-NAME
           MOVE "RET0009" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RET-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 1100-BUILD-RETURN
           MOVE SPACES TO RT-CUSTOMER
           CALL "RETURNS" USING WS-RET-CA
           MOVE "a return without a customer is refused" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-RET-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 1100-BUILD-RETURN
           MOVE SPACES TO RT-ORIG-ORDER
           MOVE SPACES TO RT-ORIG-DESPATCH
           MOVE SPACES TO RT-ORIG-INVOICE
           CALL "RETURNS" USING WS-RET-CA
           MOVE "a return must quote the original supply"
               TO TK-NAME
           MOVE "RET0001" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RET-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 1100-BUILD-RETURN
           MOVE 0 TO RT-L-REQ-QTY(1)
           CALL "RETURNS" USING WS-RET-CA
           MOVE "a return line needs a quantity above zero"
               TO TK-NAME
           MOVE "RET0005" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RET-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
      *>----------------------------------------------------------------
       1100-BUILD-RETURN.
           INITIALIZE WS-RET-CA
           MOVE "REQUEST" TO CA-FUNCTION OF WS-RET-CA
           MOVE "STC" TO CA-COMPANY OF WS-RET-CA
           MOVE "RDG" TO CA-SITE OF WS-RET-CA
           MOVE "SALESREP" TO CA-USER OF WS-RET-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-RET-CA
           MOVE "ZZSOTEST-RETURNS-0001" TO
               CA-CORRELATION-ID OF WS-RET-CA
           MOVE WS-P-CUSTOMER TO RT-CUSTOMER
           IF RT-CUSTOMER = SPACES
               MOVE "C0004" TO RT-CUSTOMER
           END-IF
           MOVE "ZZSO-ORD-0001" TO RT-ORIG-ORDER
           MOVE "ZZSO-DN-0001" TO RT-ORIG-DESPATCH
           MOVE 20261005 TO RT-DELIVERY-DATE
           MOVE 20261015 TO RT-REQUEST-DATE
           MOVE "RET-FAULT" TO RT-REASON-CODE
           MOVE "N" TO RT-IS-FAULTY
           MOVE "RDG-MAIN" TO RT-WAREHOUSE
           MOVE "QUAR-01" TO RT-QUARANTINE-LOC
           MOVE 1 TO RT-LINE-CNT
           MOVE 1 TO RT-L-LINE-NUM(1)
           MOVE WS-P-ITEM TO RT-L-ITEM(1)
           MOVE "EA" TO RT-L-UOM(1)
           MOVE 2 TO RT-L-REQ-QTY(1)
           MOVE 15 TO RT-L-ORIG-COST(1)
           MOVE 25 TO RT-L-CRED-PRICE(1)
           MOVE "S" TO RT-L-VAT-CODE(1).
      *>----------------------------------------------------------------
      *> 2000  AC-046 - AUTHORISATION ALONE CREATES NO STOCK MOVEMENT
      *>       AND NO CREDIT.  DELIVERED 10 DAYS AGO, WELL INSIDE THE
      *>       30 DAY WINDOW FOR NON FAULTY GOODS.
      *>----------------------------------------------------------------
       2000-AUTHORISATION-ALONE.
           PERFORM 1100-BUILD-RETURN
           CALL "RETURNS" USING WS-RET-CA
           IF CA-RETURN-CODE OF WS-RET-CA > 4
               MOVE "a return authorisation moves no stock"
                   TO TK-NAME
               MOVE CA-MESSAGE OF WS-RET-CA TO TK-DETAIL
               PERFORM TK-SKIP
               GO TO 2000-EXIT
           END-IF
           MOVE "a return authorisation moves no stock" TO TK-NAME
           IF RT-STOCK-MOVED = "N" AND RT-MOVEMENT-COUNT = 0
               PERFORM TK-PASS
           ELSE
               MOVE "stock was moved by a mere authorisation"
                   TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "a return authorisation raises no credit" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE RT-CREDIT-RAISED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a return does not reopen the original order"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE RT-ORDER-REOPENED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "ten days after delivery is inside the window"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE RT-WITHIN-WINDOW TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AUTHORISE still moves no stock" TO TK-NAME
           MOVE "AUTHORISE" TO CA-FUNCTION OF WS-RET-CA
           CALL "RETURNS" USING WS-RET-CA
           IF RT-STOCK-MOVED = "N" AND RT-CREDIT-RAISED = "N"
               PERFORM TK-PASS
           ELSE
               MOVE "authorising moved stock or raised a credit"
                   TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 2100  D-12 - 45 DAYS AFTER DELIVERY FOR NON FAULTY GOODS IS
      *>       OUTSIDE THE 30 DAY WINDOW.  THE REQUEST IS RECORDED BUT
      *>       HELD WITH REASON RET0004 UNTIL IT IS APPROVED.
      *>----------------------------------------------------------------
       2100-FORTY-FIVE-DAYS-LATE.
           PERFORM 1100-BUILD-RETURN
           MOVE "ZZSOTEST-RETURNS-0002" TO
               CA-CORRELATION-ID OF WS-RET-CA
           MOVE 20260901 TO RT-DELIVERY-DATE
           MOVE 20261016 TO RT-REQUEST-DATE
           MOVE "N" TO RT-IS-FAULTY
           CALL "RETURNS" USING WS-RET-CA
           MOVE "45 days is outside the 30 day window" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE RT-WITHIN-WINDOW TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a late non-faulty return is reason RET0004"
               TO TK-NAME
           MOVE "RET0004" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RET-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a late return is held for authority, not accepted"
               TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-RET-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the 45 days are counted and reported" TO TK-NAME
           MOVE 45 TO TK-EXPECTED-N
           MOVE RT-DAYS-SINCE-DEL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the non-faulty window is 30 days" TO TK-NAME
           MOVE 30 TO TK-EXPECTED-N
           MOVE RT-WINDOW-DAYS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "a late return still moves no stock" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE RT-STOCK-MOVED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
      *>----------------------------------------------------------------
      *> 2200  FAULTY GOODS GET THE WARRANTY PERIOD, DEFAULT 12
      *>       MONTHS FROM SO.RETURN.WARRANTY.MONTHS, NOT 30 DAYS.
      *>----------------------------------------------------------------
       2200-FAULTY-IN-WARRANTY.
           PERFORM 1100-BUILD-RETURN
           MOVE "ZZSOTEST-RETURNS-0003" TO
               CA-CORRELATION-ID OF WS-RET-CA
           MOVE 20260901 TO RT-DELIVERY-DATE
           MOVE 20261016 TO RT-REQUEST-DATE
           MOVE "Y" TO RT-IS-FAULTY
           CALL "RETURNS" USING WS-RET-CA
           MOVE "faulty goods use the warranty period" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE RT-WITHIN-WINDOW TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the default warranty period is 12 months"
               TO TK-NAME
           MOVE 12 TO TK-EXPECTED-N
           MOVE RT-WARRANTY-MONTHS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "a faulty return is marked as a warranty claim"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE RT-WARRANTY-CLAIM TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
      *>----------------------------------------------------------------
       2300-FAULTY-OUT-OF-WARRANTY.
           PERFORM 1100-BUILD-RETURN
           MOVE "ZZSOTEST-RETURNS-0004" TO
               CA-CORRELATION-ID OF WS-RET-CA
           MOVE 20250401 TO RT-DELIVERY-DATE
           MOVE 20261016 TO RT-REQUEST-DATE
           MOVE "Y" TO RT-IS-FAULTY
           CALL "RETURNS" USING WS-RET-CA
           MOVE "outside warranty needs an approval" TO TK-NAME
           MOVE "RET0004" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RET-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "an approval turns the hold into a warning"
               TO TK-NAME
           PERFORM 1100-BUILD-RETURN
           MOVE "ZZSOTEST-RETURNS-0005" TO
               CA-CORRELATION-ID OF WS-RET-CA
           MOVE 20250401 TO RT-DELIVERY-DATE
           MOVE 20261016 TO RT-REQUEST-DATE
           MOVE "Y" TO RT-IS-FAULTY
           MOVE 4711 TO RT-APPROVAL-ID
           CALL "RETURNS" USING WS-RET-CA
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-RET-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
      *>----------------------------------------------------------------
      *> 3000  AC-046 - A VALUE ONLY CONCESSION HAS NO STOCK EFFECT.
      *>       NEITHER CALL REACHES THE SALES LEDGER: ONE HAS NO
      *>       APPROVAL AND THE OTHER HAS NO VALUE, SO NO DATABASE IS
      *>       NEEDED TO PROVE THE STOCK ANSWER.
      *>----------------------------------------------------------------
       3000-CONCESSION-HAS-NO-STOCK.
           PERFORM 3100-BUILD-CONCESSION
           CALL "SOCREDIT" USING WS-SOCREDIT-CA
           MOVE "a concession without an approval is refused"
               TO TK-NAME
           MOVE "CRD0005" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-SOCREDIT-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a refused concession still moves no stock"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE T-STOCK-EFFECT TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 3100-BUILD-CONCESSION
      *>   approval present, but neither a value nor a percentage
           MOVE 9001 TO T-APPROVAL-ID
           MOVE "SALESMGR" TO T-APPROVED-BY
           CALL "SOCREDIT" USING WS-SOCREDIT-CA
           MOVE "a concession needs a value or a percentage"
               TO TK-NAME
           MOVE "CRD0006" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-SOCREDIT-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a value-only concession has no stock effect"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE T-STOCK-EFFECT TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a concession never returns stock to the warehouse"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE T-RETURN-STOCK TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a concession does not reopen the original order"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE T-ORDER-REOPENED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
      *>----------------------------------------------------------------
       3100-BUILD-CONCESSION.
           INITIALIZE WS-SOCREDIT-CA
           MOVE "CONCESS" TO CA-FUNCTION OF WS-SOCREDIT-CA
           MOVE "STC" TO CA-COMPANY OF WS-SOCREDIT-CA
           MOVE "RDG" TO CA-SITE OF WS-SOCREDIT-CA
           MOVE "SALESMGR" TO CA-USER OF WS-SOCREDIT-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-SOCREDIT-CA
           MOVE "ZZSOTEST-CONCESSION-0001" TO
               CA-CORRELATION-ID OF WS-SOCREDIT-CA
           MOVE "ZZSO-INV-0001" TO T-ORIG-INVOICE.
      *>----------------------------------------------------------------
       COPY TESTKITP.
