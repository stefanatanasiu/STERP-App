      *>================================================================
      *> TSOVAT  - unit tests for SOVAT, the sales line VAT liability.
      *> Every case here supplies the item's VAT classification in the
      *> COMMAREA and the de minimis in SV-DRC-DEMINIMIS, so none of
      *> these assertions needs the database.
      *>   AC-129  UK domestic supply, Northern Ireland, export of
      *>           goods with and without proof, services to an
      *>           overseas business, and the domestic reverse charge
      *>           including the case with no customer VAT number
      *> Output protocol (TAP): "ok n - text" / "not ok n - text".
      *>================================================================
      *> REQ: SO-025, SO-026, SO-003   AC: AC-129
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSOVAT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       01  WS-I                        PIC S9(9) COMP-5 VALUE 0.
       COPY SOVATCA REPLACING ==DFHCOMMAREA== BY ==WS-VAT-CA==.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TSOVAT" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-DOMESTIC
           PERFORM 2000-NORTHERN-IRELAND
           PERFORM 3000-EXPORT-OF-GOODS
           PERFORM 4000-OVERSEAS-SERVICES
           PERFORM 5000-REVERSE-CHARGE
           PERFORM 6000-BLOCKING-CONDITIONS
           PERFORM 7000-OVERRIDE
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
      *> 1000  GB DOMESTIC SUPPLY AT EACH LIABILITY
      *>----------------------------------------------------------------
       1000-DOMESTIC.
           PERFORM 9100-SET-UP
           MOVE "S" TO SV-ITEM-VAT-CODE
           PERFORM 9000-CALL-SOVAT

           MOVE "AC-129 GB domestic standard rated goods take S"
               TO TK-NAME
           MOVE "S" TO TK-EXPECTED-X
           MOVE SV-VAT-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "AC-129 the standard rate is 20%" TO TK-NAME
           MOVE 20.0000 TO TK-EXPECTED-N
           MOVE SV-VAT-RATE-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-129 VAT on 100.00 at 20% is 20.00" TO TK-NAME
           MOVE 20.00 TO TK-EXPECTED-N
           MOVE SV-VAT-AMOUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-129 a domestic supply does not block invoicing"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE SV-BLOCK-INVOICE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 9100-SET-UP
           MOVE "R" TO SV-ITEM-VAT-CODE
           PERFORM 9000-CALL-SOVAT
           MOVE "AC-129 the reduced rate is 5%" TO TK-NAME
           MOVE 5.0000 TO TK-EXPECTED-N
           MOVE SV-VAT-RATE-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 9100-SET-UP
           MOVE "Z" TO SV-ITEM-VAT-CODE
           PERFORM 9000-CALL-SOVAT
           MOVE "AC-129 a zero rated item is ZERO at 0%" TO TK-NAME
           MOVE "ZERO" TO TK-EXPECTED-X
           MOVE SV-LIABILITY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 9100-SET-UP
           MOVE "E" TO SV-ITEM-VAT-CODE
           PERFORM 9000-CALL-SOVAT
           MOVE "AC-129 an exempt item is EXEMPT, not zero rated"
               TO TK-NAME
           MOVE "EXEMPT" TO TK-EXPECTED-X
           MOVE SV-LIABILITY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> 2000  NORTHERN IRELAND IS A UK SUPPLY (D-23)
      *>----------------------------------------------------------------
       2000-NORTHERN-IRELAND.
           PERFORM 9100-SET-UP
           MOVE "XI" TO SV-CUST-LOCATION
           MOVE "XI" TO SV-DELIVER-LOCATION
           MOVE "S" TO SV-ITEM-VAT-CODE
           PERFORM 9000-CALL-SOVAT

           MOVE "AC-129 a sale to Northern Ireland keeps UK VAT"
               TO TK-NAME
           MOVE "S" TO TK-EXPECTED-X
           MOVE SV-VAT-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "AC-129 Northern Ireland is charged at 20%"
               TO TK-NAME
           MOVE 20.0000 TO TK-EXPECTED-N
           MOVE SV-VAT-RATE-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-129 Northern Ireland is not treated as an export"
               TO TK-NAME
           IF SV-VAT-CODE NOT = "EXP"
               PERFORM TK-PASS
           ELSE
               MOVE "XI was zero rated as an export" TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           PERFORM 9100-SET-UP
           MOVE "XI" TO SV-CUST-LOCATION
           MOVE "EU" TO SV-DELIVER-LOCATION
           MOVE "S" TO SV-ITEM-VAT-CODE
           PERFORM 9000-CALL-SOVAT
           MOVE "D-23 an NI to EU goods movement is out of scope"
               TO TK-NAME
           MOVE "VAT0009" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-VAT-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "D-23 the out of baseline case returns 08" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-VAT-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 3000  EXPORTS ARE ZERO RATED ONLY WITH EVIDENCE (SO-026)
      *>----------------------------------------------------------------
       3000-EXPORT-OF-GOODS.
           PERFORM 9100-SET-UP
           MOVE "ROW" TO SV-CUST-LOCATION
           MOVE "ROW" TO SV-DELIVER-LOCATION
           MOVE "US" TO SV-DELIVER-COUNTRY
           MOVE "S" TO SV-ITEM-VAT-CODE
           PERFORM 9000-CALL-SOVAT

           MOVE "AC-129 goods leaving GB are zero rated exports"
               TO TK-NAME
           MOVE "EXP" TO TK-EXPECTED-X
           MOVE SV-VAT-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "SO-026 without proof the evidence is PENDING"
               TO TK-NAME
           MOVE "PENDING" TO TK-EXPECTED-X
           MOVE SV-EVIDENCE-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "SO-026 a pending export blocks invoicing" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SV-BLOCK-INVOICE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "SO-026 a pending export is held at 06" TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-VAT-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "SO-026 the proof deadline is three months out"
               TO TK-NAME
           MOVE 20270106 TO TK-EXPECTED-N
           MOVE SV-PROOF-DEADLINE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 9100-SET-UP
           MOVE "ROW" TO SV-CUST-LOCATION
           MOVE "ROW" TO SV-DELIVER-LOCATION
           MOVE "S" TO SV-ITEM-VAT-CODE
           MOVE "Y" TO SV-PROOF-OBTAINED
           MOVE "BILLOFLADING" TO SV-PROOF-TYPE
           MOVE "MRN26GB001234" TO SV-EXPORT-DECL-REF
           PERFORM 9000-CALL-SOVAT

           MOVE "SO-026 proof of export confirms the zero rating"
               TO TK-NAME
           MOVE "OBTAINED" TO TK-EXPECTED-X
           MOVE SV-EVIDENCE-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "SO-026 a proven export does not block invoicing"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE SV-BLOCK-INVOICE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "SO-026 a proven export carries no VAT" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SV-VAT-AMOUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 9100-SET-UP
           MOVE "ROW" TO SV-CUST-LOCATION
           MOVE "ROW" TO SV-DELIVER-LOCATION
           MOVE "S" TO SV-ITEM-VAT-CODE
           MOVE 20260101 TO SV-SUPPLY-DATE
           MOVE 20260101 TO SV-DESPATCH-DATE
           PERFORM 9000-CALL-SOVAT

           MOVE "SO-026 proof overdue raises VAT0005" TO TK-NAME
           MOVE "VAT0005" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-VAT-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "SO-026 an overdue export is OVERDUE, not zero"
               TO TK-NAME
           MOVE "OVERDUE" TO TK-EXPECTED-X
           MOVE SV-EVIDENCE-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> 4000  SERVICES TO AN OVERSEAS BUSINESS CUSTOMER
      *>----------------------------------------------------------------
       4000-OVERSEAS-SERVICES.
           PERFORM 9100-SET-UP
           MOVE "SERVICE" TO SV-LINE-TYPE
           MOVE "EU" TO SV-CUST-LOCATION
           MOVE "EU" TO SV-DELIVER-LOCATION
           MOVE "S" TO SV-ITEM-VAT-CODE
           MOVE "Y" TO SV-CUST-IS-BUSINESS
           MOVE "IE1234567X" TO SV-CUST-VAT-REG-NO
           PERFORM 9000-CALL-SOVAT

           MOVE "AC-129 services to an overseas business are O"
               TO TK-NAME
           MOVE "O" TO TK-EXPECTED-X
           MOVE SV-VAT-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "AC-129 the supply is outside the scope of UK VAT"
               TO TK-NAME
           MOVE "OUTSIDE" TO TK-EXPECTED-X
           MOVE SV-LIABILITY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "AC-129 the customer accounts for any VAT due"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SV-SELF-ACCOUNTS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "AC-129 the invoice wording names the customer"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           IF SV-INVOICE-WORDING(1:60) = SPACES
               MOVE "N" TO TK-ACTUAL-X
           ELSE
               MOVE "Y" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR

           MOVE "AC-129 no UK VAT is charged on an outside supply"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SV-VAT-AMOUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 9100-SET-UP
           MOVE "SERVICE" TO SV-LINE-TYPE
           MOVE "EU" TO SV-CUST-LOCATION
           MOVE "EU" TO SV-DELIVER-LOCATION
           MOVE "S" TO SV-ITEM-VAT-CODE
           MOVE "N" TO SV-CUST-IS-BUSINESS
           PERFORM 9000-CALL-SOVAT
           MOVE "SO-025 services to an overseas consumer stay UK"
               TO TK-NAME
           MOVE "S" TO TK-EXPECTED-X
           MOVE SV-VAT-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> 5000  THE DOMESTIC REVERSE CHARGE FOR SPECIFIED GOODS
      *>----------------------------------------------------------------
       5000-REVERSE-CHARGE.
           PERFORM 9100-SET-UP
           MOVE "S" TO SV-ITEM-VAT-CODE
           MOVE "Y" TO SV-ITEM-DRC-GOODS
           MOVE "Y" TO SV-CUST-DRC-ELIG
           MOVE "GB123456789" TO SV-CUST-VAT-REG-NO
           MOVE 6000.00 TO SV-NET-AMOUNT
           MOVE 6000.00 TO SV-NET-AMOUNT-GBP
           PERFORM 9000-CALL-SOVAT

           MOVE "AC-129 specified goods above 5000 take DRC"
               TO TK-NAME
           MOVE "DRC" TO TK-EXPECTED-X
           MOVE SV-VAT-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "AC-129 the DRC customer accounts for the VAT"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SV-SELF-ACCOUNTS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "AC-129 the supplier charges no VAT under the DRC"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SV-VAT-AMOUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 9100-SET-UP
           MOVE "S" TO SV-ITEM-VAT-CODE
           MOVE "Y" TO SV-ITEM-DRC-GOODS
           MOVE "Y" TO SV-CUST-DRC-ELIG
           MOVE "GB123456789" TO SV-CUST-VAT-REG-NO
           MOVE 4000.00 TO SV-NET-AMOUNT
           MOVE 4000.00 TO SV-NET-AMOUNT-GBP
           PERFORM 9000-CALL-SOVAT

           MOVE "AC-129 below the 5000 de minimis the DRC is off"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE SV-DRC-APPLIES TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "AC-129 below de minimis the normal 20% applies"
               TO TK-NAME
           MOVE 20.0000 TO TK-EXPECTED-N
           MOVE SV-VAT-RATE-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 6000  A MISSING CONDITION BLOCKS - IT DOES NOT DEFAULT TO ZERO
      *>----------------------------------------------------------------
       6000-BLOCKING-CONDITIONS.
           PERFORM 9100-SET-UP
           MOVE "S" TO SV-ITEM-VAT-CODE
           MOVE "Y" TO SV-ITEM-DRC-GOODS
           MOVE "Y" TO SV-CUST-DRC-ELIG
           MOVE SPACES TO SV-CUST-VAT-REG-NO
           MOVE 6000.00 TO SV-NET-AMOUNT
           MOVE 6000.00 TO SV-NET-AMOUNT-GBP
           PERFORM 9000-CALL-SOVAT

           MOVE "AC-129 a reverse charge with no VAT number is 08"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-VAT-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-129 the reason is VAT0003" TO TK-NAME
           MOVE "VAT0003" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-VAT-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "AC-129 no VAT number blocks invoicing" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SV-BLOCK-INVOICE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 9100-SET-UP
           MOVE SPACES TO SV-ITEM-VAT-CODE
           MOVE "N" TO SV-READ-MASTER
           PERFORM 9000-CALL-SOVAT
           MOVE "SO-003 no item classification is rejected VAT0002"
               TO TK-NAME
           MOVE "VAT0002" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-VAT-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 9100-SET-UP
           MOVE "S" TO SV-ITEM-VAT-CODE
           MOVE "ROW" TO SV-DELIVER-LOCATION
           MOVE "S" TO SV-OVERRIDE-CODE
           PERFORM 9000-CALL-SOVAT
           MOVE "SO-026 a domestic code for an export needs evidence"
               TO TK-NAME
           MOVE "VAT0006" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-VAT-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 9100-SET-UP
           MOVE "S" TO SV-ITEM-VAT-CODE
           MOVE "ZZ" TO SV-CUST-LOCATION
           PERFORM 9000-CALL-SOVAT
           MOVE "SO-025 an unknown VAT location is rejected"
               TO TK-NAME
           MOVE "VAT0011" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-VAT-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 9100-SET-UP
           MOVE "S" TO SV-ITEM-VAT-CODE
           MOVE "GUESS" TO CA-FUNCTION OF WS-VAT-CA
           PERFORM 9000-CALL-SOVAT
           MOVE "SO-025 an unsupported function is rejected"
               TO TK-NAME
           MOVE "VAT0008" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-VAT-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> 7000  AN AUTHORISED OVERRIDE IS RECORDED WITH ITS REASON
      *>----------------------------------------------------------------
       7000-OVERRIDE.
           PERFORM 9100-SET-UP
           MOVE "OVERRIDE" TO CA-FUNCTION OF WS-VAT-CA
           MOVE "S" TO SV-ITEM-VAT-CODE
           MOVE "Z" TO SV-OVERRIDE-CODE
           MOVE SPACES TO SV-OVERRIDE-REASON
           PERFORM 9000-CALL-SOVAT

           MOVE "SO-025 an override with no reason is rejected"
               TO TK-NAME
           MOVE "VAT0007" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-VAT-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 9100-SET-UP
           MOVE "OVERRIDE" TO CA-FUNCTION OF WS-VAT-CA
           MOVE "S" TO SV-ITEM-VAT-CODE
           MOVE "Z" TO SV-OVERRIDE-CODE
           MOVE "Zero rated food ruling of 14 May 2026"
               TO SV-OVERRIDE-REASON
           MOVE "APPR000912" TO SV-OVERRIDE-APPROVL
           MOVE "VATOWNER" TO SV-OVERRIDE-USER
           PERFORM 9000-CALL-SOVAT

           MOVE "SO-025 an authorised override is applied" TO TK-NAME
           MOVE "Z" TO TK-EXPECTED-X
           MOVE SV-VAT-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "SO-025 the override is recorded" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SV-OVERRIDE-APPLIED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "SO-025 the overridden rate follows the new code"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SV-VAT-RATE-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "SO-025 the rule trace explains the decision"
               TO TK-NAME
           IF SV-RULE-CNT > 0
               PERFORM TK-PASS
           ELSE
               MOVE "no rule trace was returned" TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> 9000  SHARED SET UP - GB COMPANY, GB CUSTOMER, 100.00 OF
      *>       GOODS ON 6 OCTOBER 2026, NO DATABASE NEEDED
      *>----------------------------------------------------------------
       9000-CALL-SOVAT.
           CALL "SOVAT" USING WS-VAT-CA.

       9100-SET-UP.
           INITIALIZE WS-VAT-CA
           MOVE "DECIDE" TO CA-FUNCTION OF WS-VAT-CA
           MOVE "STC" TO CA-COMPANY OF WS-VAT-CA
           MOVE "RDG" TO CA-SITE OF WS-VAT-CA
           MOVE "VATOWNER" TO CA-USER OF WS-VAT-CA
           MOVE 20261006 TO CA-BUSINESS-DATE OF WS-VAT-CA
           MOVE "SALESORDER" TO SV-DOC-TYPE
           MOVE "SO100129" TO SV-DOC-REF
           MOVE 1 TO SV-LINE-NUM
           MOVE "WID-001" TO SV-ITEM-CODE
           MOVE "GOODS" TO SV-LINE-TYPE
           MOVE 20261006 TO SV-SUPPLY-DATE
           MOVE 20261006 TO SV-DESPATCH-DATE
           MOVE 100.00 TO SV-NET-AMOUNT
           MOVE 100.00 TO SV-NET-AMOUNT-GBP
           MOVE "GBP" TO SV-CURRENCY
           MOVE "C000001" TO SV-CUSTOMER
           MOVE "GB" TO SV-CUST-LOCATION
           MOVE "GB" TO SV-CUST-COUNTRY
           MOVE "GB123456789" TO SV-CUST-VAT-REG-NO
           MOVE "Y" TO SV-CUST-IS-BUSINESS
           MOVE "N" TO SV-CUST-DRC-ELIG
           MOVE "GB" TO SV-DELIVER-LOCATION
           MOVE "GB" TO SV-DELIVER-COUNTRY
           MOVE "GB" TO SV-PLACE-OF-SUPPLY
           MOVE "N" TO SV-ITEM-DRC-GOODS
           MOVE "N" TO SV-READ-MASTER
           MOVE 3 TO SV-EVIDENCE-MONTHS
           MOVE 5000.00 TO SV-DRC-DEMINIMIS.

       COPY TESTKITP.
