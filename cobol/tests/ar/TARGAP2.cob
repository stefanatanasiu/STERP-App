      *>================================================================
      *> PROGRAM-ID : TARGAP2
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR) UNIT TESTS
      *> PURPOSE    : DISHONOR AND ARINSOLV - A DISHONOURED RECEIPT
      *>              REINSTATES EXACTLY THE DEBT IT PAID, ACROSS
      *>              EVERY ALLOCATION IT TOUCHED, WITH THE BANK FEE
      *>              KEPT SEPARATE; AND AN INSOLVENCY OR RECOVERY
      *>              EVENT IS RECORDED WITHOUT EVER REMOVING THE
      *>              DEBT FROM THE LEDGER.  NOTHING HERE COMMITS.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2026-10-07 STERP ORIGINAL - CLOSING THE AR TEST GAPS
      *>================================================================
      *> REQ: AR-014, AR-027
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TARGAP2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY DISHONORCA REPLACING ==DFHCOMMAREA== BY ==WS-DH-CA==.
       COPY ARINSOLVCA REPLACING ==DFHCOMMAREA== BY ==WS-IS-CA==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "N".
       01  WS-ROW.
           05  WS-R1                   PIC X(30).
           05  WS-R2                   PIC X(30).
       01  WS-WORK.
           05  WS-RCPT                 PIC X(30) VALUE SPACES.
           05  WS-RCPT-GBP             PIC S9(13)V99 VALUE 0.
           05  WS-RCPT-STATE           PIC X(20) VALUE SPACES.
           05  WS-RCPT-STATE-2         PIC X(20) VALUE SPACES.
           05  WS-DEBTOR               PIC X(20) VALUE SPACES.
           05  WS-OUTST-BEFORE         PIC S9(13)V99 VALUE 0.
           05  WS-OUTST-AFTER          PIC S9(13)V99 VALUE 0.
           05  WS-ROT-SUM              PIC S9(13)V99 VALUE 0.
           05  WS-RESID-NOW            PIC S9(13)V99 VALUE 0.
           05  WS-RESID-EXP            PIC S9(13)V99 VALUE 0.
           05  WS-DIV-EXP              PIC S9(13)V99 VALUE 0.
           05  WS-I                    PIC S9(4) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TARGAP2 dishonoured receipts and insolvency"
               TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP NOT = "Y"
               PERFORM 0900-SKIP-EVERYTHING
           ELSE
               PERFORM 1000-DISHONOUR-EDITS
               PERFORM 1100-REINSTATES-THE-WHOLE-RECEIPT
               PERFORM 1150-RESIDUAL-AFTER-IS-REPORTED
               PERFORM 1200-BANK-FEE-IS-SEPARATE
               PERFORM 2000-INSOLVENCY-EVIDENCE
               PERFORM 2100-PRE-ACTION-PROTOCOL
               PERFORM 2200-RETENTION-OF-TITLE
               PERFORM 2300-DIVIDEND-IN-STERLING
           END-IF
           PERFORM TK-END
           STOP RUN.

       0100-PROBE-DATABASE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) ReceiptNumber, "
                  "CAST(GrossReceivedGBP AS varchar(30)) FROM "
                  "ar.Receipt WHERE CompanyCode = 'STC' AND "
                  "Status = 'POSTED' ORDER BY ReceiptId"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-SQLCODE = 0 AND WS-R1 NOT = SPACES
               MOVE WS-R1 TO WS-RCPT
               COMPUTE WS-RCPT-GBP = FUNCTION NUMVAL(WS-R2)
               MOVE "Y" TO WS-DB-UP
           END-IF
           IF WS-DB-UP = "Y"
               MOVE SPACES TO SQLIO-STMT
               STRING "SELECT TOP (1) CustomerCode, "
                      "CAST(SUM(ResidualAmountGBP) AS varchar(30)) "
                      "FROM ar.OpenItem WHERE CompanyCode = 'STC' "
                      "AND ResidualAmountGBP > 0 GROUP BY "
                      "CustomerCode ORDER BY SUM(ResidualAmountGBP) "
                      "DESC"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 0 TO SQLIO-PARM-CNT
               MOVE 2 TO SQLIO-COL-CNT
               MOVE 30 TO SQLIO-COL-WIDTH(1)
               MOVE 30 TO SQLIO-COL-WIDTH(2)
               MOVE 1 TO SQLIO-CURSOR
               MOVE "SNGL" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ROW
               MOVE WS-R1 TO WS-DEBTOR
               COMPUTE WS-OUTST-BEFORE = FUNCTION NUMVAL(WS-R2)
               IF WS-DEBTOR = SPACES MOVE "N" TO WS-DB-UP END-IF
           END-IF.

       0900-SKIP-EVERYTHING.
           MOVE "no database or no posted receipt is seeded"
               TO TK-DETAIL
           MOVE "AR-014 dishonoured receipts" TO TK-NAME
           PERFORM TK-SKIP
           MOVE "no database or no open debt is seeded" TO TK-DETAIL
           MOVE "AR-027 insolvency and debt recovery" TO TK-NAME
           PERFORM TK-SKIP.

       8200-ONE-VALUE.
           MOVE SPACES TO WS-R1 WS-R2
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

       8300-PRIME-DISHONOUR.
           INITIALIZE WS-DH-CA
           MOVE "STC" TO CA-COMPANY OF WS-DH-CA
           MOVE "RDG" TO CA-SITE OF WS-DH-CA
           MOVE "ARCLERK1" TO CA-USER OF WS-DH-CA
           MOVE 20261007 TO CA-BUSINESS-DATE OF WS-DH-CA
           MOVE WS-RCPT TO DH-RECEIPT-NO
           MOVE "BANK" TO DH-SOURCE
           MOVE 20261007 TO DH-DISHONOUR-DATE
           MOVE 20261007 TO DH-POSTING-DATE
           MOVE "Returned unpaid - refer to drawer" TO DH-REASON.

       8400-PRIME-INSOLVENCY.
           INITIALIZE WS-IS-CA
           MOVE "STC" TO CA-COMPANY OF WS-IS-CA
           MOVE "RDG" TO CA-SITE OF WS-IS-CA
           MOVE "CREDMGR" TO CA-USER OF WS-IS-CA
           MOVE 20261007 TO CA-BUSINESS-DATE OF WS-IS-CA
           MOVE WS-DEBTOR TO IS-CUSTOMER
           MOVE "ADMIN" TO IS-EVENT-TYPE
           MOVE 20261001 TO IS-EVENT-DATE
           MOVE "Begbies Traynor (Reading) LLP" TO IS-OFFICE-HOLDER.

      *>   AR-014.  A dishonour is a reversal of a payment that has
      *>   already been relied on, so the instruction has to name the
      *>   receipt and say why before anything is undone.
       1000-DISHONOUR-EDITS.
           PERFORM 8300-PRIME-DISHONOUR
           MOVE "WIBBLE" TO CA-FUNCTION OF WS-DH-CA
           CALL "DISHONOR" USING WS-DH-CA
           MOVE "An unknown dishonour function is refused" TO TK-NAME
           MOVE "DIS0098" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-DH-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8300-PRIME-DISHONOUR
           MOVE "ASSESS" TO CA-FUNCTION OF WS-DH-CA
           MOVE SPACES TO DH-RECEIPT-NO
           MOVE 0 TO DH-RECEIPT-ID
           CALL "DISHONOR" USING WS-DH-CA
           MOVE "A dishonour must name the receipt (DIS0002)"
               TO TK-NAME
           MOVE "DIS0002" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-DH-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8300-PRIME-DISHONOUR
           MOVE "ASSESS" TO CA-FUNCTION OF WS-DH-CA
           MOVE "ZZ-NO-SUCH-RECEIPT" TO DH-RECEIPT-NO
           CALL "DISHONOR" USING WS-DH-CA
           MOVE "An unknown receipt is reported as not found"
               TO TK-NAME
           MOVE 16 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-DH-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 8300-PRIME-DISHONOUR
           MOVE "DISHONR" TO CA-FUNCTION OF WS-DH-CA
           MOVE SPACES TO DH-REASON
           CALL "DISHONOR" USING WS-DH-CA
           MOVE "A dishonour without a reason is refused" TO TK-NAME
           MOVE "DIS0003" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-DH-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AR-014.  The debt reinstated is exactly the receipt, and
      *>   every open item the receipt was allocated to is corrected,
      *>   not just the first one.  Assessing changes nothing.
       1100-REINSTATES-THE-WHOLE-RECEIPT.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Status FROM ar.Receipt WHERE CompanyCode "
                  "= 'STC' AND ReceiptNumber = '"
               DELIMITED BY SIZE
               FUNCTION TRIM(WS-RCPT) DELIMITED BY SIZE
               "'" DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8200-ONE-VALUE
           MOVE WS-R1 TO WS-RCPT-STATE
           PERFORM 8300-PRIME-DISHONOUR
           MOVE "ASSESS" TO CA-FUNCTION OF WS-DH-CA
           CALL "DISHONOR" USING WS-DH-CA
           MOVE "The receipt amount is read from the ledger"
               TO TK-NAME
           MOVE WS-RCPT-GBP TO TK-EXPECTED-N
           MOVE DH-RECEIPT-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The debt reinstated is exactly the receipt amount"
               TO TK-NAME
           MOVE DH-RECEIPT-GBP TO TK-EXPECTED-N
           MOVE DH-REINSTATED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "Every allocation the receipt made is reversed"
               TO TK-NAME
           MOVE DH-ITEM-CNT TO TK-EXPECTED-N
           MOVE DH-ALLOC-REVERSED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Status FROM ar.Receipt WHERE CompanyCode "
                  "= 'STC' AND ReceiptNumber = '"
               DELIMITED BY SIZE
               FUNCTION TRIM(WS-RCPT) DELIMITED BY SIZE
               "'" DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8200-ONE-VALUE
           MOVE WS-R1 TO WS-RCPT-STATE-2
           MOVE "Assessing a dishonour changes no receipt" TO TK-NAME
           MOVE WS-RCPT-STATE TO TK-EXPECTED-X
           MOVE WS-RCPT-STATE-2 TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AR-014.  The bank's charge for handling the returned item
      *>   is the bank's charge; it is never netted off the debt the
      *>   customer still owes.
       1200-BANK-FEE-IS-SEPARATE.
           PERFORM 8300-PRIME-DISHONOUR
           MOVE "ASSESS" TO CA-FUNCTION OF WS-DH-CA
           MOVE 25.00 TO DH-BANK-FEE
           MOVE "7905" TO DH-FEE-ACCOUNT
           CALL "DISHONOR" USING WS-DH-CA
           MOVE "A bank fee is not netted off the reinstated debt"
               TO TK-NAME
           MOVE WS-RCPT-GBP TO TK-EXPECTED-N
           MOVE DH-REINSTATED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The bank fee is carried separately" TO TK-NAME
           MOVE 25.00 TO TK-EXPECTED-N
           MOVE DH-BANK-FEE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AR-027.  An insolvency event is a dated, evidenced fact
      *>   about a named office holder; a vague one is refused.
       2000-INSOLVENCY-EVIDENCE.
           PERFORM 8400-PRIME-INSOLVENCY
           MOVE "WIBBLE" TO CA-FUNCTION OF WS-IS-CA
           CALL "ARINSOLV" USING WS-IS-CA
           MOVE "An unknown insolvency function is refused"
               TO TK-NAME
           MOVE "INS0009" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-IS-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8400-PRIME-INSOLVENCY
           MOVE "RECORD" TO CA-FUNCTION OF WS-IS-CA
           MOVE "RECEIVERSHIP" TO IS-EVENT-TYPE
           CALL "ARINSOLV" USING WS-IS-CA
           MOVE "Only the recognised insolvency events are accepted"
               TO TK-NAME
           MOVE "INS0003" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-IS-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8400-PRIME-INSOLVENCY
           MOVE "RECORD" TO CA-FUNCTION OF WS-IS-CA
           MOVE SPACES TO IS-OFFICE-HOLDER
           CALL "ARINSOLV" USING WS-IS-CA
           MOVE "An insolvency event must name the office holder"
               TO TK-NAME
           MOVE "INS0004" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-IS-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8400-PRIME-INSOLVENCY
           MOVE "RECORD" TO CA-FUNCTION OF WS-IS-CA
           MOVE 20271231 TO IS-EVENT-DATE
           CALL "ARINSOLV" USING WS-IS-CA
           MOVE "An insolvency event cannot be dated in the future"
               TO TK-NAME
           MOVE "INS0005" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-IS-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AR-027.  Before an individual debtor is handed to a
      *>   solicitor or an agency the Pre-Action Protocol letter of
      *>   claim has to be evidenced, and the reply period can never
      *>   be shortened below the thirty days the protocol allows.
      *>   A blocked hand-off leaves the ledger exactly as it was.
       2100-PRE-ACTION-PROTOCOL.
           PERFORM 8400-PRIME-INSOLVENCY
           MOVE "HANDOFF" TO CA-FUNCTION OF WS-IS-CA
           MOVE SPACES TO IS-HANDOFF-REF
           CALL "ARINSOLV" USING WS-IS-CA
           MOVE "A hand-off must name the agency or solicitor"
               TO TK-NAME
           MOVE "INS0006" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-IS-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8400-PRIME-INSOLVENCY
           MOVE "HANDOFF" TO CA-FUNCTION OF WS-IS-CA
           MOVE "BANKRUPTCY" TO IS-EVENT-TYPE
           MOVE "AGENCY" TO IS-HANDOFF-TYPE
           MOVE "Lovetts Solicitors, ref LVT/2026/118"
               TO IS-HANDOFF-REF
           MOVE 0 TO IS-PAP-SENT-DATE
           CALL "ARINSOLV" USING WS-IS-CA
           MOVE "An individual debtor is not handed over" TO TK-NAME
           MOVE "INS0050" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-IS-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "The blocked hand-off is held for authority," TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-IS-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The protocol reply period is never under 30 days"
               TO TK-NAME
           IF IS-PAP-DAYS-REQ >= 30
               PERFORM TK-PASS
           ELSE
               MOVE IS-PAP-DAYS-REQ TO TK-ACTUAL-N
               MOVE 30 TO TK-EXPECTED-N
               PERFORM TK-ASSERT-NUM
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(SUM(ResidualAmountGBP), 0) AS "
                  "varchar(30)) FROM ar.OpenItem WHERE CompanyCode "
                  "= 'STC' AND CustomerCode = '"
               DELIMITED BY SIZE
               FUNCTION TRIM(WS-DEBTOR) DELIMITED BY SIZE
               "'" DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8200-ONE-VALUE
           COMPUTE WS-OUTST-AFTER = FUNCTION NUMVAL(WS-R1)
           MOVE "Handing the debt over never removes it from" TO TK-NAME
           MOVE WS-OUTST-BEFORE TO TK-EXPECTED-N
           MOVE WS-OUTST-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AR-027.  Goods supplied under retention of title are
      *>   listed with their value so a claim can be made; listing
      *>   them is an enquiry and writes nothing off.
       2200-RETENTION-OF-TITLE.
           PERFORM 8400-PRIME-INSOLVENCY
           MOVE "ROTLIST" TO CA-FUNCTION OF WS-IS-CA
           CALL "ARINSOLV" USING WS-IS-CA
           MOVE "A retention of title list is produced" TO TK-NAME
           IF CA-RETURN-CODE OF WS-IS-CA < 8
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-IS-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE 0 TO WS-ROT-SUM
           IF IS-ROT-CNT > 0
               PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > IS-ROT-CNT
                   ADD IS-R-VALUE (WS-I) TO WS-ROT-SUM
               END-PERFORM
           END-IF
           MOVE "The claim value is the sum of the goods listed"
               TO TK-NAME
           MOVE WS-ROT-SUM TO TK-EXPECTED-N
           MOVE IS-ROT-VALUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(SUM(ResidualAmountGBP), 0) AS "
                  "varchar(30)) FROM ar.OpenItem WHERE CompanyCode "
                  "= 'STC' AND CustomerCode = '"
               DELIMITED BY SIZE
               FUNCTION TRIM(WS-DEBTOR) DELIMITED BY SIZE
               "'" DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8200-ONE-VALUE
           COMPUTE WS-OUTST-AFTER = FUNCTION NUMVAL(WS-R1)
           MOVE "Listing goods under retention of title" TO TK-NAME
           MOVE WS-OUTST-BEFORE TO TK-EXPECTED-N
           MOVE WS-OUTST-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AR-014.  Each reinstated item has to say what is left open
      *>   on it once the dishonoured allocation comes off, so the
      *>   collector knows what to chase without reading the ledger.
       1150-RESIDUAL-AFTER-IS-REPORTED.
           PERFORM 8300-PRIME-DISHONOUR
           MOVE "ASSESS" TO CA-FUNCTION OF WS-DH-CA
           CALL "DISHONOR" USING WS-DH-CA
           IF DH-ITEM-CNT = 0
               MOVE "the seeded receipt has no live allocation"
                   TO TK-DETAIL
               MOVE "AR-014 residual after a dishonour" TO TK-NAME
               PERFORM TK-SKIP
           ELSE
               MOVE SPACES TO SQLIO-STMT
               STRING "SELECT CAST(ResidualAmount AS varchar(30)) "
                      "FROM ar.OpenItem WHERE CompanyCode = 'STC' "
                      "AND DocumentNumber = '"
                   DELIMITED BY SIZE
                   FUNCTION TRIM(DH-I-DOC-NO (1)) DELIMITED BY SIZE
                   "'" DELIMITED BY SIZE INTO SQLIO-STMT
               PERFORM 8200-ONE-VALUE
               COMPUTE WS-RESID-NOW = FUNCTION NUMVAL(WS-R1)
               COMPUTE WS-RESID-EXP = WS-RESID-NOW
                   + DH-I-AMOUNT (1)
               MOVE "The item is left owing what the receipt paid"
                   TO TK-NAME
               MOVE WS-RESID-EXP TO TK-EXPECTED-N
               MOVE DH-I-RESID-AFTER (1) TO TK-ACTUAL-N
               PERFORM TK-ASSERT-NUM
               MOVE "A reinstated item reports an open amount"
                   TO TK-NAME
               IF DH-I-RESID-AFTER (1) >= DH-I-AMOUNT (1)
                  AND DH-I-RESID-AFTER (1) > 0
                   PERFORM TK-PASS
               ELSE
                   MOVE DH-I-AMOUNT (1) TO TK-EXPECTED-N
                   MOVE DH-I-RESID-AFTER (1) TO TK-ACTUAL-N
                   PERFORM TK-ASSERT-NUM
               END-IF
               MOVE "Assessing leaves the item exactly as it was"
                   TO TK-NAME
               MOVE SPACES TO SQLIO-STMT
               STRING "SELECT CAST(ResidualAmount AS varchar(30)) "
                      "FROM ar.OpenItem WHERE CompanyCode = 'STC' "
                      "AND DocumentNumber = '"
                   DELIMITED BY SIZE
                   FUNCTION TRIM(DH-I-DOC-NO (1)) DELIMITED BY SIZE
                   "'" DELIMITED BY SIZE INTO SQLIO-STMT
               PERFORM 8200-ONE-VALUE
               MOVE WS-RESID-NOW TO TK-EXPECTED-N
               COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-R1)
               PERFORM TK-ASSERT-NUM
           END-IF.

      *>   AR-027.  A dividend from the office holder is stated in
      *>   the currency it arrived in and reported in sterling; it
      *>   never writes the debt off by itself.
       2300-DIVIDEND-IN-STERLING.
           PERFORM 8400-PRIME-INSOLVENCY
           MOVE "ROTLIST" TO CA-FUNCTION OF WS-IS-CA
           CALL "ARINSOLV" USING WS-IS-CA
           MOVE "No dividend means nothing is reported" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE IS-DIVIDEND-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 8400-PRIME-INSOLVENCY
           MOVE "ROTLIST" TO CA-FUNCTION OF WS-IS-CA
           MOVE 250.00 TO IS-DIVIDEND-AMT
           MOVE "GBP" TO IS-DIVIDEND-CCY
           MOVE 20261001 TO IS-DIVIDEND-DATE
           CALL "ARINSOLV" USING WS-IS-CA
           MOVE "A sterling dividend is reported as it stands"
               TO TK-NAME
           MOVE 250.00 TO TK-EXPECTED-N
           MOVE IS-DIVIDEND-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(CONVERT(decimal(18,2), ROUND(1250.00 "
                  "/ UnitsPerGBP, 2)) AS varchar(30)) FROM "
                  "core.FxRate WHERE CurrencyCode = 'EUR' AND "
                  "RateType = 'SPOT' AND RateDate = '20261001'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8200-ONE-VALUE
           IF WS-R1 = SPACES
               MOVE "no EUR spot rate is seeded for 01/10/2026"
                   TO TK-DETAIL
               MOVE "AR-027 a foreign dividend is converted"
                   TO TK-NAME
               PERFORM TK-SKIP
           ELSE
               COMPUTE WS-DIV-EXP = FUNCTION NUMVAL(WS-R1)
               PERFORM 8400-PRIME-INSOLVENCY
               MOVE "ROTLIST" TO CA-FUNCTION OF WS-IS-CA
               MOVE 1250.00 TO IS-DIVIDEND-AMT
               MOVE "EUR" TO IS-DIVIDEND-CCY
               MOVE 20261001 TO IS-DIVIDEND-DATE
               CALL "ARINSOLV" USING WS-IS-CA
               MOVE "A foreign dividend is converted at the rate"
                   TO TK-NAME
               MOVE WS-DIV-EXP TO TK-EXPECTED-N
               MOVE IS-DIVIDEND-GBP TO TK-ACTUAL-N
               PERFORM TK-ASSERT-NUM
           END-IF
           PERFORM 8400-PRIME-INSOLVENCY
           MOVE "ROTLIST" TO CA-FUNCTION OF WS-IS-CA
           MOVE -100.00 TO IS-DIVIDEND-AMT
           CALL "ARINSOLV" USING WS-IS-CA
           MOVE "A negative dividend is refused" TO TK-NAME
           MOVE "INS0041" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-IS-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(SUM(ResidualAmountGBP), 0) AS "
                  "varchar(30)) FROM ar.OpenItem WHERE CompanyCode "
                  "= 'STC' AND CustomerCode = '"
               DELIMITED BY SIZE
               FUNCTION TRIM(WS-DEBTOR) DELIMITED BY SIZE
               "'" DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8200-ONE-VALUE
           COMPUTE WS-OUTST-AFTER = FUNCTION NUMVAL(WS-R1)
           MOVE "A dividend writes nothing off the ledger" TO TK-NAME
           MOVE WS-OUTST-BEFORE TO TK-EXPECTED-N
           MOVE WS-OUTST-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       COPY TESTKITP.
