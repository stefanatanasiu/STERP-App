      *>================================================================
      *> PROGRAM-ID : EXPEVID
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : EXPORT AND CUSTOMS EVIDENCE FOR A CONFIRMED
      *>              DESPATCH.  ZERO RATING AN EXPORT OF GOODS IS
      *>              CONDITIONAL ON COMMERCIAL AND OFFICIAL EVIDENCE
      *>              BEING OBTAINED WITHIN THE PERMITTED PERIOD,
      *>              BASELINE THREE MONTHS FROM THE SUPPLY (SO-026).
      *>              AN EXPORT THAT REACHES ITS DEADLINE WITH NO
      *>              PROOF BECOMES OVERDUE, RAISES AN EXCEPTION
      *>              THROUGH EXCPRAIS AND REPORTS THE VAT THAT MUST
      *>              NOW BE ACCOUNTED FOR.
      *>              A DESPATCH TO AN EU OR REST OF WORLD ADDRESS MAY
      *>              NOT USE A DOMESTIC VAT CODE WITHOUT AUTHORISED
      *>              EVIDENCE (EXP0003).  GREAT BRITAIN TO NORTHERN
      *>              IRELAND GOODS ARE UK DOMESTIC SUPPLIES (D-23).
      *>              A DESPATCH LINE WITH NO COMMODITY CODE BLOCKS
      *>              THE CUSTOMS HAND OFF (EXP0007).
      *> CALLED BY  : STERP DESKTOP CLIENT (EXPORT EVIDENCE), SOVATDUE
      *> INTERFACE  : CALL "EXPEVID" USING DFHCOMMAREA (COPY EXPEVIDCA)
      *> CALLS      : DATECALC, PARMGET, EXCPRAIS, SQLIO -> so.Despatch
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-06-07 RJH  CR0288 EC SALES AND EXPORT DOCUMENTATION
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT YEARS ON EVIDENCE DATES
      *> 2021-01-04 DLS  CR3004 BREXIT - GB EXPORTS AND NI PROTOCOL
      *> 2026-10-06 STERP SO-025 SO-026 D-23 PROOF OF EXPORT DEADLINE
      *>================================================================
      *> REQ: SO-025, SO-026
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXPEVID.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 07/06/1994.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DATE-CA==.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARM-CA==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXC-CA==.
       01  WS-CONTROL.
           05  WS-FUNC                 PIC X(8).
           05  WS-CO                   PIC X(10).
           05  WS-SITE                 PIC X(10).
           05  WS-USER                 PIC X(20).
           05  WS-BDATE                PIC 9(8).
           05  WS-CORR                 PIC X(36).
           05  WS-ERROR                PIC X VALUE "N".
           05  WS-IN-TXN               PIC X VALUE "N".
           05  WS-RC                   PIC 9(2) VALUE 0.
           05  WS-REASON               PIC X(8).
           05  WS-MSG                  PIC X(200).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-N                    PIC S9(15)V9(6) VALUE 0.
           05  WS-AMT1                 PIC S9(13)V99 VALUE 0.
           05  WS-AMT2                 PIC S9(13)V99 VALUE 0.
           05  WS-RATE                 PIC S9(3)V9(4) VALUE 0.
           05  WS-ROWS                 PIC S9(4) VALUE 0.
           05  WS-BLOCKED              PIC S9(4) VALUE 0.
           05  WS-AMT-ED               PIC -(11)9.99.
           05  WS-INT-ED               PIC -(8)9.
           05  WS-DATE-ED              PIC X(10).
      *>   the EU member states, kept here because a change of
      *>   membership is a change of law, not a change of data
       01  WS-EU-TABLE.
           05  FILLER                  PIC X(54) VALUE
               "ATBEBGHRCYCZDKEEFIFRDEGRHUIEITLVLTLUMTNLPLPTROSKSIESSE".
       01  FILLER REDEFINES WS-EU-TABLE.
           05  WS-EU-CODE              PIC X(2) OCCURS 27 TIMES.
       01  WS-HEAD-ROW.
           05  WD-DESPATCH-ID          PIC X(24).
           05  WD-CUSTOMER             PIC X(20).
           05  WD-ORDER                PIC X(30).
           05  WD-DESP-DATE            PIC X(8).
           05  WD-COUNTRY              PIC X(2).
           05  WD-IS-EXPORT            PIC X(2).
           05  WD-EORI                 PIC X(17).
           05  WD-DECLARATION          PIC X(40).
           05  WD-PROOF-TYPE           PIC X(30).
           05  WD-PROOF-DATE           PIC X(8).
           05  WD-DEADLINE             PIC X(8).
           05  WD-EV-STATUS            PIC X(12).
           05  WD-INCOTERM             PIC X(3).
           05  WD-PLACE                PIC X(60).
           05  WD-NET-GBP              PIC X(24).
       01  WS-LINE-ROW.
           05  WX-LINE-NUM             PIC X(12).
           05  WX-ITEM                 PIC X(40).
           05  WX-DESC                 PIC X(60).
           05  WX-QTY                  PIC X(24).
           05  WX-COMMODITY            PIC X(10).
           05  WX-ORIGIN               PIC X(2).
           05  WX-CUST-VALUE           PIC X(24).
       01  WS-DUE-ROW.
           05  WU-DESP-REF             PIC X(30).
           05  WU-CUSTOMER             PIC X(20).
           05  WU-DESP-DATE            PIC X(8).
           05  WU-DEADLINE             PIC X(8).
           05  WU-COUNTRY              PIC X(2).
           05  WU-STATUS               PIC X(12).
           05  WU-NET-GBP              PIC X(24).
       01  WS-ONE-COL.
           05  WS-C1                   PIC X(40).
           05  WS-C2                   PIC X(40).
       LINKAGE SECTION.
       COPY EXPEVIDCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 1000-INITIALISE THRU 1000-EXIT
           IF WS-ERROR = "N"
               EVALUATE WS-FUNC
                   WHEN "CAPTURE"
                       PERFORM 2000-CAPTURE THRU 2000-EXIT
                   WHEN "RECORDPF "
                       PERFORM 3000-RECORD-PROOF THRU 3000-EXIT
                   WHEN "CHECKDUE"
                       PERFORM 4000-CHECK-DUE THRU 4000-EXIT
                   WHEN "STATUS"
                       PERFORM 5000-STATUS THRU 5000-EXIT
                   WHEN OTHER
                       PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
               END-EVALUATE
           END-IF
           PERFORM 9400-REPORT THRU 9400-EXIT.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INITIALISE SECTION.
       1000-START.
           MOVE "N" TO WS-ERROR
           MOVE "N" TO WS-IN-TXN
           MOVE 0 TO WS-RC WS-I WS-J WS-ROWS WS-BLOCKED
           MOVE SPACES TO WS-REASON WS-MSG
           MOVE "EXPEVID" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "SO-026" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE FUNCTION UPPER-CASE(CA-FUNCTION OF DFHCOMMAREA)
               TO WS-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO WS-CO
           MOVE CA-SITE OF DFHCOMMAREA TO WS-SITE
           MOVE CA-USER OF DFHCOMMAREA TO WS-USER
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO WS-BDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO WS-CORR
           MOVE "N" TO XE-CUSTOMS-BLOCKED
           MOVE "N" TO XE-IS-OVERDUE
           MOVE 0 TO XE-EXCEPTION-COUNT
           MOVE 0 TO XE-TOTAL-VAT-DUE
           IF WS-BDATE = 0
               MOVE 20261006 TO WS-BDATE
               MOVE WS-BDATE TO CA-BUSINESS-DATE OF DFHCOMMAREA
           END-IF
           IF XE-AS-AT-DATE = 0
               MOVE WS-BDATE TO XE-AS-AT-DATE
           END-IF
           IF XE-LINE-CNT > 20
               MOVE 20 TO XE-LINE-CNT
           END-IF
           IF WS-CO = SPACES
               MOVE "EXP0001" TO WS-REASON
               MOVE "A company code is required" TO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1000-EXIT
           END-IF
           PERFORM 1100-PROOF-MONTHS THRU 1100-EXIT.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1100-PROOF-MONTHS SECTION.
       1100-START.
           IF XE-PROOF-MONTHS > 0
               GO TO 1100-EXIT
           END-IF
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE WS-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE WS-USER TO CA-USER OF WS-PARM-CA
           MOVE WS-BDATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "SO.EXPORT.PROOF.MONTHS" TO PM-PARAM-CODE
           MOVE "Y" TO PM-USE-DEFAULT
           MOVE "3" TO PM-DEFAULT-TEXT
           CALL "PARMGET" USING WS-PARM-CA
           MOVE PM-VALUE-NUM TO WS-N
           MOVE WS-N TO XE-PROOF-MONTHS
           IF XE-PROOF-MONTHS <= 0
               MOVE 3 TO XE-PROOF-MONTHS
           END-IF.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 1200  WHERE THE GOODS WENT DECIDES THE VAT QUESTION.  GB AND
      *>       XI ARE BOTH UK DOMESTIC SUPPLIES OF GOODS (D-23); AN EU
      *>       MEMBER STATE IS EU; ANYTHING ELSE IS REST OF WORLD.
      *>----------------------------------------------------------------
       1200-CLASSIFY SECTION.
       1200-START.
           MOVE FUNCTION UPPER-CASE(XE-DEST-COUNTRY)
               TO XE-DEST-COUNTRY
           EVALUATE XE-DEST-COUNTRY
               WHEN "GB"
                   MOVE "GB" TO XE-DEST-REGION
               WHEN "XI"
                   MOVE "NI" TO XE-DEST-REGION
               WHEN "NI"
                   MOVE "NI" TO XE-DEST-REGION
               WHEN OTHER
                   MOVE "ROW" TO XE-DEST-REGION
                   PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 27
                       IF WS-EU-CODE(WS-J) = XE-DEST-COUNTRY
                           MOVE "EU" TO XE-DEST-REGION
                       END-IF
                   END-PERFORM
           END-EVALUATE
           IF XE-REGION-GB OR XE-REGION-NI
               MOVE "Y" TO XE-IS-DOMESTIC
               MOVE "N" TO XE-IS-EXPORT
           ELSE
               MOVE "N" TO XE-IS-DOMESTIC
               MOVE "Y" TO XE-IS-EXPORT
           END-IF.
       1200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 2000  CAPTURE.  THE CUSTOMS AND EVIDENCE FACTS ARE RECORDED
      *>       AGAINST THE DESPATCH AND ITS LINES.  NOTHING HERE
      *>       CHANGES THE PHYSICAL FULFILMENT - IT IS THE PAPERWORK
      *>       THAT MAKES ZERO RATING DEFENSIBLE.
      *>----------------------------------------------------------------
       2000-CAPTURE SECTION.
       2000-START.
           PERFORM 8000-LOAD-DESPATCH THRU 8000-EXIT
           IF WS-ERROR = "Y"
               GO TO 2000-EXIT
           END-IF
           IF XE-DEST-COUNTRY = SPACES
               MOVE "EXP0002" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A destination country is required before "
                      "the customs declaration can be prepared"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           PERFORM 1200-CLASSIFY THRU 1200-EXIT
           PERFORM 2100-VAT-CODE-TEST THRU 2100-EXIT
           IF WS-ERROR = "Y"
               GO TO 2000-EXIT
           END-IF
           IF XE-EXPORT
               IF XE-INCOTERMS-RULE = SPACES OR
                  XE-INCOTERMS-PLACE = SPACES
                   MOVE "EXP0004" TO WS-REASON
                   MOVE SPACES TO WS-MSG
                   STRING "An export states the Incoterms rule and "
                          "the named place on the commercial invoice"
                       DELIMITED BY SIZE INTO WS-MSG
                   PERFORM 9000-REJECT THRU 9000-EXIT
                   GO TO 2000-EXIT
               END-IF
               IF XE-CUSTOMER-EORI = SPACES
                   MOVE "EXP0005" TO WS-REASON
                   MOVE SPACES TO WS-MSG
                   STRING "The customer's EORI number is needed for "
                          "the export declaration"
                       DELIMITED BY SIZE INTO WS-MSG
                   IF WS-RC < 4
                       MOVE 4 TO WS-RC
                   END-IF
               END-IF
           END-IF
           PERFORM 8100-LOAD-LINES THRU 8100-EXIT
           IF WS-ERROR = "Y"
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-CHECK-LINES THRU 2200-EXIT
           PERFORM 8600-BEGIN THRU 8600-EXIT
           IF WS-ERROR = "Y"
               GO TO 2000-EXIT
           END-IF
           PERFORM 2300-WRITE-LINES THRU 2300-EXIT
           IF WS-ERROR = "Y"
               PERFORM 8620-ROLLBACK THRU 8620-EXIT
               GO TO 2000-EXIT
           END-IF
           PERFORM 2400-WRITE-HEADER THRU 2400-EXIT
           IF WS-ERROR = "Y"
               PERFORM 8620-ROLLBACK THRU 8620-EXIT
               GO TO 2000-EXIT
           END-IF
           PERFORM 8610-COMMIT THRU 8610-EXIT
           IF XE-CUSTOMS-BLOCKED = "Y"
               MOVE "EXP0007" TO WS-REASON
               MOVE WS-BLOCKED TO WS-INT-ED
               MOVE SPACES TO WS-MSG
               STRING "The customs hand-off is blocked: "
                      FUNCTION TRIM(WS-INT-ED)
                      " despatch line(s) have no commodity code"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           MOVE SPACES TO WS-MSG
           STRING "Export evidence captured for "
                  FUNCTION TRIM(XE-DESPATCH-REF)
                  "; proof of export is due by "
               DELIMITED BY SIZE INTO WS-MSG
           MOVE XE-PROOF-DEADLINE TO WS-DATE-ED
           STRING FUNCTION TRIM(WS-MSG) " "
                  FUNCTION TRIM(WS-DATE-ED)
               DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 2100  A DOMESTIC VAT CODE ON AN EU OR REST OF WORLD SUPPLY IS
      *>       REFUSED UNLESS THERE IS AUTHORISED EVIDENCE BEHIND IT.
      *>----------------------------------------------------------------
       2100-VAT-CODE-TEST SECTION.
       2100-START.
           IF NOT XE-EXPORT
               GO TO 2100-EXIT
           END-IF
           IF XE-VAT-CODE = SPACES
               MOVE "EXP" TO XE-VAT-CODE
               GO TO 2100-EXIT
           END-IF
           IF XE-VAT-CODE = "EXP" OR XE-VAT-CODE = "Z" OR
              XE-VAT-CODE = "O" OR XE-VAT-CODE = "E"
               GO TO 2100-EXIT
           END-IF
           IF XE-EVIDENCE-REF NOT = SPACES AND
              XE-OVERRIDE-APPROVL > 0
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
               MOVE "EXP0003" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A domestic VAT code has been allowed on this "
                      "overseas supply under authorised evidence "
                      FUNCTION TRIM(XE-EVIDENCE-REF)
                   DELIMITED BY SIZE INTO WS-MSG
               GO TO 2100-EXIT
           END-IF
           MOVE "EXP0003" TO WS-REASON
           MOVE SPACES TO WS-MSG
           STRING "VAT code " FUNCTION TRIM(XE-VAT-CODE)
                  " is a domestic code and cannot be used for a "
                  "despatch to " FUNCTION TRIM(XE-DEST-COUNTRY)
                  " without authorised evidence"
               DELIMITED BY SIZE INTO WS-MSG
           PERFORM 9000-REJECT THRU 9000-EXIT.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2200-CHECK-LINES SECTION.
       2200-START.
           MOVE 0 TO WS-BLOCKED
           MOVE 0 TO XE-CUSTOMS-VAL-GBP
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > XE-LINE-CNT
               MOVE "N" TO XE-L-BLOCKED(WS-I)
               MOVE SPACES TO XE-L-MESSAGE(WS-I)
               ADD XE-L-CUST-VALUE(WS-I) TO XE-CUSTOMS-VAL-GBP
               IF XE-L-COMMODITY(WS-I) = SPACES
                   MOVE "Y" TO XE-L-BLOCKED(WS-I)
                   ADD 1 TO WS-BLOCKED
                   STRING "No commodity code; this line cannot be "
                          "declared to customs"
                       DELIMITED BY SIZE INTO XE-L-MESSAGE(WS-I)
               ELSE
                   IF XE-L-ORIGIN(WS-I) = SPACES OR
                      XE-L-CUST-VALUE(WS-I) = 0
                       STRING "Country of origin or customs value "
                              "is still missing"
                           DELIMITED BY SIZE INTO XE-L-MESSAGE(WS-I)
                       IF WS-RC < 4
                           MOVE 4 TO WS-RC
                           MOVE "EXP0010" TO WS-REASON
                           MOVE XE-L-MESSAGE(WS-I) TO WS-MSG
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           IF WS-BLOCKED > 0
               MOVE "Y" TO XE-CUSTOMS-BLOCKED
           ELSE
               MOVE "N" TO XE-CUSTOMS-BLOCKED
           END-IF.
       2200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2300-WRITE-LINES SECTION.
       2300-START.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > XE-LINE-CNT OR WS-ERROR = "Y"
               MOVE SPACES TO SQLIO-STMT
               STRING "UPDATE so.DespatchLine SET CommodityCode = "
                      "NULLIF(?,''), CountryOfOrigin = NULLIF(?,''), "
                      "CustomsValueGBP = NULLIF(?,'0') WHERE "
                      "DespatchId = ? AND LineNum = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE XE-L-COMMODITY(WS-I) TO SQLIO-PARM-VAL(1)
               MOVE XE-L-ORIGIN(WS-I) TO SQLIO-PARM-VAL(2)
               MOVE XE-L-CUST-VALUE(WS-I) TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT)
                   TO SQLIO-PARM-VAL(3)
               MOVE XE-DESPATCH-ID TO WS-INT-ED
               MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(4)
               MOVE XE-L-LINE-NUM(WS-I) TO WS-INT-ED
               MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(5)
               MOVE 5 TO SQLIO-PARM-CNT
               PERFORM 8650-NO-NULLS THRU 8650-EXIT
               PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           END-PERFORM.
       2300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2400-WRITE-HEADER SECTION.
       2400-START.
           IF XE-SUPPLY-DATE = 0
               MOVE XE-DESPATCH-DATE TO XE-SUPPLY-DATE
           END-IF
           PERFORM 8200-SET-DEADLINE THRU 8200-EXIT
           IF XE-EXPORT
               IF XE-PROOF-DATE > 0
                   MOVE "OBTAINED" TO XE-EVIDENCE-STATUS
               ELSE
                   MOVE "PENDING" TO XE-EVIDENCE-STATUS
               END-IF
           ELSE
               MOVE "NA" TO XE-EVIDENCE-STATUS
               MOVE 0 TO XE-PROOF-DEADLINE
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.Despatch SET IncotermsRule = "
                  "NULLIF(?,''), IncotermsNamedPlace = NULLIF(?,''), "
                  "IsExport = ?, DestinationCountry = NULLIF(?,''), "
                  "CustomerEori = NULLIF(?,''), ExportDeclarationRef "
                  "= NULLIF(?,''), ProofDeadlineDate = "
                  "CASE WHEN ? = '0' THEN NULL ELSE CONVERT(date, ?, "
                  "112) END, ExportEvidenceStatus = ?, ModifiedBy = "
                  "?, ModifiedAt = SYSDATETIME() WHERE DespatchId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XE-INCOTERMS-RULE TO SQLIO-PARM-VAL(1)
           MOVE XE-INCOTERMS-PLACE TO SQLIO-PARM-VAL(2)
           IF XE-EXPORT
               MOVE "1" TO SQLIO-PARM-VAL(3)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(3)
           END-IF
           MOVE XE-DEST-COUNTRY TO SQLIO-PARM-VAL(4)
           MOVE XE-CUSTOMER-EORI TO SQLIO-PARM-VAL(5)
           MOVE XE-DECLARATION-REF TO SQLIO-PARM-VAL(6)
           MOVE XE-PROOF-DEADLINE TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(7)
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(8)
           MOVE XE-EVIDENCE-STATUS TO SQLIO-PARM-VAL(9)
           MOVE WS-USER TO SQLIO-PARM-VAL(10)
           MOVE XE-DESPATCH-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(11)
           MOVE 11 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       2400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 3000  RECORDPF .  THE PROOF TYPE AND DATE ARE RECORDED AND
      *>       TESTED AGAINST THE DEADLINE.  PROOF OBTAINED AFTER THE
      *>       PERMITTED PERIOD DOES NOT RESCUE THE ZERO RATE: IT IS
      *>       REPORTED AS EXP0008 AND THE EVIDENCE FAILS.
      *>----------------------------------------------------------------
       3000-RECORD-PROOF SECTION.
       3000-START.
           PERFORM 8000-LOAD-DESPATCH THRU 8000-EXIT
           IF WS-ERROR = "Y"
               GO TO 3000-EXIT
           END-IF
           PERFORM 1200-CLASSIFY THRU 1200-EXIT
           IF NOT XE-EXPORT
               MOVE 4 TO WS-RC
               MOVE "NA" TO XE-EVIDENCE-STATUS
               MOVE SPACES TO WS-MSG
               STRING "Despatch " FUNCTION TRIM(XE-DESPATCH-REF)
                      " is a UK domestic supply; no proof of export "
                      "is required"
                   DELIMITED BY SIZE INTO WS-MSG
               GO TO 3000-EXIT
           END-IF
           IF XE-PROOF-TYPE = SPACES OR XE-PROOF-DATE = 0
               MOVE "EXP0006" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Record both the type of proof of export and "
                      "the date it was obtained"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           IF XE-SUPPLY-DATE = 0
               MOVE XE-DESPATCH-DATE TO XE-SUPPLY-DATE
           END-IF
           PERFORM 8200-SET-DEADLINE THRU 8200-EXIT
           IF XE-PROOF-DATE > XE-PROOF-DEADLINE
               MOVE "FAILED" TO XE-EVIDENCE-STATUS
               MOVE "Y" TO XE-IS-OVERDUE
               PERFORM 8300-VAT-THAT-FALLS-DUE THRU 8300-EXIT
               MOVE "EXP0008" TO WS-REASON
               MOVE XE-PROOF-DEADLINE TO WS-DATE-ED
               MOVE SPACES TO WS-MSG
               STRING "The proof of export was obtained after the "
                      "permitted period, which ended on "
                      FUNCTION TRIM(WS-DATE-ED)
                      "; VAT must be accounted for"
                   DELIMITED BY SIZE INTO WS-MSG
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
           ELSE
               MOVE "OBTAINED" TO XE-EVIDENCE-STATUS
               MOVE "N" TO XE-IS-OVERDUE
               MOVE 0 TO XE-VAT-DUE-GBP
           END-IF
           PERFORM 8400-UPDATE-EVIDENCE THRU 8400-EXIT
           IF WS-ERROR = "Y"
               GO TO 3000-EXIT
           END-IF
           IF XE-EV-OBTAINED
               MOVE SPACES TO WS-MSG
               STRING "Proof of export "
                      FUNCTION TRIM(XE-PROOF-TYPE)
                      " recorded within the permitted period; the "
                      "zero rate is evidenced"
                   DELIMITED BY SIZE INTO WS-MSG
               MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
               MOVE SPACES TO WS-MSG
           END-IF.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 4000  CHECKDUE.  EVERY PENDING EXPORT WHOSE DEADLINE HAS BEEN
      *>       REACHED WITH NO PROOF BECOMES OVERDUE, RAISES AN
      *>       EXCEPTION FOR THE VAT OWNER AND REPORTS THE VAT THAT
      *>       FALLS DUE.  THIS IS THE THREE MONTH TEST OF AC-129.
      *>----------------------------------------------------------------
       4000-CHECK-DUE SECTION.
       4000-START.
           MOVE 0 TO XE-DUE-CNT
           MOVE "N" TO XE-DUE-TRUNCATED
           MOVE 0 TO XE-TOTAL-VAT-DUE
           PERFORM 4050-STANDARD-RATE THRU 4050-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT d.DespatchRef, d.CustomerCode, "
                  "CONVERT(char(8), d.DespatchDate, 112), "
                  "ISNULL(CONVERT(char(8), d.ProofDeadlineDate, "
                  "112),''), ISNULL(d.DestinationCountry,''), "
                  "d.ExportEvidenceStatus, CAST(ISNULL((SELECT "
                  "SUM(i.NetAmountGBP) FROM ar.SalesInvoice i WHERE "
                  "i.CompanyCode = d.CompanyCode AND i.DespatchRef = "
                  "d.DespatchRef AND i.DocumentType = 'INVOICE'), "
                  "(SELECT ISNULL(SUM(dl.CustomsValueGBP),0) FROM "
                  "so.DespatchLine dl WHERE dl.DespatchId = "
                  "d.DespatchId)) AS varchar(24)) FROM so.Despatch d "
                  "WHERE d.CompanyCode = ? AND d.IsExport = 1 AND "
                  "d.Status = 'CONFIRMED' AND "
                  "d.ExportEvidenceStatus IN ('PENDING','OVERDUE') "
                  "AND d.ProofObtainedDate IS NULL AND "
                  "d.ProofDeadlineDate IS NOT NULL AND "
                  "d.ProofDeadlineDate <= CONVERT(date, ?, 112) "
                  "ORDER BY d.ProofDeadlineDate, d.DespatchRef"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE XE-AS-AT-DATE TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           MOVE 7 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-COL-WIDTH(3)
           MOVE 8 TO SQLIO-COL-WIDTH(4)
           MOVE 2 TO SQLIO-COL-WIDTH(5)
           MOVE 12 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-DUE-ROW
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
               GO TO 4000-EXIT
           END-IF
           MOVE 0 TO WS-ROWS
           PERFORM 4100-ONE-DUE THRU 4100-EXIT
               UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR OR WS-ROWS >= 40
           IF NOT SQLIO-NOTFOUND AND NOT SQLIO-ERROR
               MOVE "Y" TO XE-DUE-TRUNCATED
           END-IF
           MOVE 3 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-DUE-ROW
           MOVE WS-ROWS TO XE-DUE-CNT
           IF WS-ROWS = 0
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               STRING "No export is past its proof of export "
                      "deadline"
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               GO TO 4000-EXIT
           END-IF
           MOVE XE-TOTAL-VAT-DUE TO WS-AMT-ED
           MOVE WS-ROWS TO WS-INT-ED
           MOVE SPACES TO WS-MSG
           STRING FUNCTION TRIM(WS-INT-ED)
                  " export(s) have reached the proof of export "
                  "deadline with no evidence; VAT of £"
                  FUNCTION TRIM(WS-AMT-ED)
                  " must now be accounted for"
               DELIMITED BY SIZE INTO WS-MSG
           IF WS-RC < 4
               MOVE 4 TO WS-RC
           END-IF
           MOVE "EXP0008" TO WS-REASON.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4050-STANDARD-RATE SECTION.
       4050-START.
           IF XE-VAT-RATE-PCT > 0
               MOVE XE-VAT-RATE-PCT TO WS-RATE
               GO TO 4050-EXIT
           END-IF
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE WS-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE WS-USER TO CA-USER OF WS-PARM-CA
           MOVE XE-AS-AT-DATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "SO.EXPORT.VAT.FALLBACK.RATE" TO PM-PARAM-CODE
           MOVE "Y" TO PM-USE-DEFAULT
           MOVE "20" TO PM-DEFAULT-TEXT
           CALL "PARMGET" USING WS-PARM-CA
           MOVE PM-VALUE-NUM TO WS-N
           MOVE WS-N TO WS-RATE
           IF WS-RATE <= 0
               MOVE 20 TO WS-RATE
           END-IF
           MOVE WS-RATE TO XE-VAT-RATE-PCT.
       4050-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4100-ONE-DUE SECTION.
       4100-START.
           MOVE 3 TO SQLIO-CURSOR
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-DUE-ROW
           IF SQLIO-NOTFOUND OR SQLIO-ERROR
               GO TO 4100-EXIT
           END-IF
           ADD 1 TO WS-ROWS
           MOVE WS-ROWS TO WS-I
           MOVE WU-DESP-REF TO XE-D-DESP-REF(WS-I)
           MOVE WU-CUSTOMER TO XE-D-CUSTOMER(WS-I)
           COMPUTE WS-N = FUNCTION NUMVAL(WU-DESP-DATE)
           MOVE WS-N TO XE-D-DESP-DATE(WS-I)
           IF WU-DEADLINE NOT = SPACES
               COMPUTE WS-N = FUNCTION NUMVAL(WU-DEADLINE)
               MOVE WS-N TO XE-D-DEADLINE(WS-I)
           END-IF
           MOVE WU-COUNTRY TO XE-D-COUNTRY(WS-I)
           MOVE "OVERDUE" TO XE-D-STATUS(WS-I)
           COMPUTE WS-N = FUNCTION NUMVAL(WU-NET-GBP)
           MOVE WS-N TO WS-AMT1
           MOVE WS-AMT1 TO XE-D-NET-GBP(WS-I)
           COMPUTE WS-AMT2 ROUNDED = WS-AMT1 * WS-RATE / 100
           MOVE WS-AMT2 TO XE-D-VAT-DUE(WS-I)
           ADD WS-AMT2 TO XE-TOTAL-VAT-DUE
           MOVE SPACES TO XE-D-MESSAGE(WS-I)
           STRING "No proof of export by the deadline; VAT falls due"
               DELIMITED BY SIZE INTO XE-D-MESSAGE(WS-I)
           PERFORM 4200-MARK-OVERDUE THRU 4200-EXIT
           PERFORM 4300-RAISE-EXCEPTION THRU 4300-EXIT.
       4100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4200-MARK-OVERDUE SECTION.
       4200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.Despatch SET ExportEvidenceStatus = "
                  "'OVERDUE', ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() WHERE CompanyCode = ? AND "
                  "DespatchRef = ? AND ExportEvidenceStatus = "
                  "'PENDING'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-USER TO SQLIO-PARM-VAL(1)
           MOVE WS-CO TO SQLIO-PARM-VAL(2)
           MOVE XE-D-DESP-REF(WS-I) TO SQLIO-PARM-VAL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       4200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4300-RAISE-EXCEPTION SECTION.
       4300-START.
           INITIALIZE WS-EXC-CA
           MOVE "RAISE" TO CA-FUNCTION OF WS-EXC-CA
           MOVE WS-CO TO CA-COMPANY OF WS-EXC-CA
           MOVE WS-SITE TO CA-SITE OF WS-EXC-CA
           MOVE WS-USER TO CA-USER OF WS-EXC-CA
           MOVE XE-AS-AT-DATE TO CA-BUSINESS-DATE OF WS-EXC-CA
           MOVE WS-CORR TO CA-CORRELATION-ID OF WS-EXC-CA
           MOVE "SO" TO EX-MODULE
           MOVE "ERROR" TO EX-SEVERITY
           MOVE "EXPORT.PROOF.OVERDUE" TO EX-CATEGORY
           MOVE XE-D-VAT-DUE(WS-I) TO WS-AMT-ED
           MOVE SPACES TO EX-MESSAGE-TEXT
           STRING "Despatch " FUNCTION TRIM(XE-D-DESP-REF(WS-I))
                  " to " FUNCTION TRIM(XE-D-COUNTRY(WS-I))
                  " has no proof of export by the permitted "
                  "deadline; VAT of £" FUNCTION TRIM(WS-AMT-ED)
                  " must be accounted for"
               DELIMITED BY SIZE INTO EX-MESSAGE-TEXT
           MOVE "DESPATCH" TO EX-DOCUMENT-TYPE
           MOVE XE-D-DESP-REF(WS-I) TO EX-DOCUMENT-REF
           MOVE "VATOWNER" TO EX-OWNER-ROLE
           MOVE 1 TO EX-PRIORITY
           MOVE XE-D-DEADLINE(WS-I) TO EX-DUE-AT
           MOVE XE-D-VAT-DUE(WS-I) TO EX-AMOUNT-GBP
           CALL "EXCPRAIS" USING WS-EXC-CA
           IF CA-RETURN-CODE OF WS-EXC-CA < 8
               ADD 1 TO XE-EXCEPTION-COUNT
               MOVE EX-EXCEPTION-ID TO XE-EXCEPTION-ID
           END-IF.
       4300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       5000-STATUS SECTION.
       5000-START.
           PERFORM 8000-LOAD-DESPATCH THRU 8000-EXIT
           IF WS-ERROR = "Y"
               GO TO 5000-EXIT
           END-IF
           PERFORM 1200-CLASSIFY THRU 1200-EXIT
           PERFORM 8100-LOAD-LINES THRU 8100-EXIT
           IF WS-ERROR = "Y"
               GO TO 5000-EXIT
           END-IF
           PERFORM 2200-CHECK-LINES THRU 2200-EXIT
           MOVE 0 TO WS-RC
           MOVE SPACES TO WS-REASON
           MOVE SPACES TO WS-MSG
           IF XE-PROOF-DEADLINE > 0 AND XE-PROOF-DATE = 0
               PERFORM 8250-DAYS-TO-DEADLINE THRU 8250-EXIT
               IF XE-AS-AT-DATE > XE-PROOF-DEADLINE
                   MOVE "OVERDUE" TO XE-EVIDENCE-STATUS
                   MOVE "Y" TO XE-IS-OVERDUE
                   PERFORM 4050-STANDARD-RATE THRU 4050-EXIT
                   PERFORM 8300-VAT-THAT-FALLS-DUE THRU 8300-EXIT
                   MOVE "EXP0008" TO WS-REASON
                   MOVE XE-VAT-DUE-GBP TO WS-AMT-ED
                   STRING "No proof of export is held and the "
                          "deadline has passed; VAT of £"
                          FUNCTION TRIM(WS-AMT-ED)
                          " falls due"
                       DELIMITED BY SIZE INTO WS-MSG
                   MOVE 4 TO WS-RC
               END-IF
           END-IF
           IF XE-CUSTOMS-BLOCKED = "Y" AND WS-RC < 4
               MOVE 4 TO WS-RC
               MOVE "EXP0007" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A despatch line has no commodity code; the "
                      "customs hand-off is blocked"
                   DELIMITED BY SIZE INTO WS-MSG
           END-IF.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8000-LOAD-DESPATCH SECTION.
       8000-START.
           IF XE-DESPATCH-REF = SPACES
               MOVE "EXP0001" TO WS-REASON
               MOVE "A despatch reference is required" TO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 8000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(d.DespatchId AS varchar(24)), "
                  "d.CustomerCode, ISNULL(d.OrderNumber,''), "
                  "CONVERT(char(8), d.DespatchDate, 112), "
                  "ISNULL(d.DestinationCountry,''), CAST(d.IsExport "
                  "AS char(1)), ISNULL(d.CustomerEori,''), "
                  "ISNULL(d.ExportDeclarationRef,''), "
                  "ISNULL(d.ProofOfExportType,''), "
                  "ISNULL(CONVERT(char(8), d.ProofObtainedDate, "
                  "112),''), ISNULL(CONVERT(char(8), "
                  "d.ProofDeadlineDate, 112),''), "
                  "d.ExportEvidenceStatus, ISNULL(d.IncotermsRule,"
                  "''), ISNULL(d.IncotermsNamedPlace,''), "
                  "CAST(ISNULL((SELECT SUM(i.NetAmountGBP) FROM "
                  "ar.SalesInvoice i WHERE i.CompanyCode = "
                  "d.CompanyCode AND i.DespatchRef = d.DespatchRef "
                  "AND i.DocumentType = 'INVOICE'), (SELECT "
                  "ISNULL(SUM(dl.CustomsValueGBP),0) FROM "
                  "so.DespatchLine dl WHERE dl.DespatchId = "
                  "d.DespatchId)) AS "
                  "varchar(24)) FROM so.Despatch d WHERE "
                  "d.CompanyCode = ? AND d.DespatchRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE XE-DESPATCH-REF TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           MOVE 15 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 30 TO SQLIO-COL-WIDTH(3)
           MOVE 8 TO SQLIO-COL-WIDTH(4)
           MOVE 2 TO SQLIO-COL-WIDTH(5)
           MOVE 2 TO SQLIO-COL-WIDTH(6)
           MOVE 17 TO SQLIO-COL-WIDTH(7)
           MOVE 40 TO SQLIO-COL-WIDTH(8)
           MOVE 30 TO SQLIO-COL-WIDTH(9)
           MOVE 8 TO SQLIO-COL-WIDTH(10)
           MOVE 8 TO SQLIO-COL-WIDTH(11)
           MOVE 12 TO SQLIO-COL-WIDTH(12)
           MOVE 3 TO SQLIO-COL-WIDTH(13)
           MOVE 60 TO SQLIO-COL-WIDTH(14)
           MOVE 24 TO SQLIO-COL-WIDTH(15)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-HEAD-ROW
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
               GO TO 8000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO WS-RC
               MOVE "Y" TO WS-ERROR
               MOVE "EXP0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Despatch " FUNCTION TRIM(XE-DESPATCH-REF)
                      " is not held for this company"
                   DELIMITED BY SIZE INTO WS-MSG
               GO TO 8000-EXIT
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WD-DESPATCH-ID)
           MOVE WS-N TO XE-DESPATCH-ID
           MOVE WD-CUSTOMER TO XE-CUSTOMER
           MOVE WD-ORDER TO XE-ORDER-NUMBER
           COMPUTE WS-N = FUNCTION NUMVAL(WD-DESP-DATE)
           MOVE WS-N TO XE-DESPATCH-DATE
           IF XE-DEST-COUNTRY = SPACES
               MOVE WD-COUNTRY TO XE-DEST-COUNTRY
           END-IF
           IF XE-CUSTOMER-EORI = SPACES
               MOVE WD-EORI TO XE-CUSTOMER-EORI
           END-IF
           IF XE-DECLARATION-REF = SPACES
               MOVE WD-DECLARATION TO XE-DECLARATION-REF
           END-IF
           IF XE-INCOTERMS-RULE = SPACES
               MOVE WD-INCOTERM TO XE-INCOTERMS-RULE
           END-IF
           IF XE-INCOTERMS-PLACE = SPACES
               MOVE WD-PLACE TO XE-INCOTERMS-PLACE
           END-IF
           IF XE-PROOF-TYPE = SPACES
               MOVE WD-PROOF-TYPE TO XE-PROOF-TYPE
           END-IF
           IF XE-PROOF-DATE = 0 AND WD-PROOF-DATE NOT = SPACES
               COMPUTE WS-N = FUNCTION NUMVAL(WD-PROOF-DATE)
               MOVE WS-N TO XE-PROOF-DATE
           END-IF
           IF WD-DEADLINE NOT = SPACES
               COMPUTE WS-N = FUNCTION NUMVAL(WD-DEADLINE)
               MOVE WS-N TO XE-PROOF-DEADLINE
           END-IF
           MOVE WD-EV-STATUS TO XE-EVIDENCE-STATUS
           COMPUTE WS-N = FUNCTION NUMVAL(WD-NET-GBP)
           IF XE-NET-GBP = 0
               MOVE WS-N TO XE-NET-GBP
           END-IF
           IF XE-SUPPLY-DATE = 0
               MOVE XE-DESPATCH-DATE TO XE-SUPPLY-DATE
           END-IF.
       8000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8100-LOAD-LINES SECTION.
       8100-START.
           IF XE-LINE-CNT > 0
               GO TO 8100-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(LineNum AS varchar(12)), ItemCode, "
                  "ISNULL(Description,''), CAST(DespatchedQty AS "
                  "varchar(24)), ISNULL(CommodityCode,''), "
                  "ISNULL(CountryOfOrigin,''), "
                  "ISNULL(CAST(CustomsValueGBP AS varchar(24)),'0') "
                  "FROM so.DespatchLine WHERE DespatchId = ? "
                  "ORDER BY LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XE-DESPATCH-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           MOVE 7 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 60 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 10 TO SQLIO-COL-WIDTH(5)
           MOVE 2 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
               GO TO 8100-EXIT
           END-IF
           MOVE 0 TO WS-ROWS
           PERFORM 8150-FETCH-LINE THRU 8150-EXIT
               UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR OR WS-ROWS >= 20
           MOVE 2 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
           MOVE WS-ROWS TO XE-LINE-CNT.
       8100-EXIT.
           EXIT.

       8150-FETCH-LINE SECTION.
       8150-START.
           MOVE 2 TO SQLIO-CURSOR
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
           IF SQLIO-NOTFOUND OR SQLIO-ERROR
               GO TO 8150-EXIT
           END-IF
           ADD 1 TO WS-ROWS
           MOVE WS-ROWS TO WS-J
           COMPUTE WS-N = FUNCTION NUMVAL(WX-LINE-NUM)
           MOVE WS-N TO XE-L-LINE-NUM(WS-J)
           MOVE WX-ITEM TO XE-L-ITEM(WS-J)
           MOVE WX-DESC TO XE-L-DESC(WS-J)
           COMPUTE WS-N = FUNCTION NUMVAL(WX-QTY)
           MOVE WS-N TO XE-L-QTY(WS-J)
           MOVE WX-COMMODITY TO XE-L-COMMODITY(WS-J)
           MOVE WX-ORIGIN TO XE-L-ORIGIN(WS-J)
           COMPUTE WS-N = FUNCTION NUMVAL(WX-CUST-VALUE)
           MOVE WS-N TO XE-L-CUST-VALUE(WS-J).
       8150-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 8200  THE PERMITTED PERIOD RUNS FROM THE SUPPLY, NOT FROM THE
      *>       INVOICE, AND IS MEASURED IN WHOLE MONTHS SO 31 JANUARY
      *>       PLUS THREE MONTHS IS 30 APRIL.
      *>----------------------------------------------------------------
       8200-SET-DEADLINE SECTION.
       8200-START.
           INITIALIZE WS-DATE-CA
           MOVE "ADDMONTH" TO CA-FUNCTION OF WS-DATE-CA
           MOVE WS-CO TO CA-COMPANY OF WS-DATE-CA
           MOVE WS-USER TO CA-USER OF WS-DATE-CA
           MOVE XE-SUPPLY-DATE TO DC-DATE-1
           MOVE XE-PROOF-MONTHS TO DC-NUMBER
           CALL "DATECALC" USING WS-DATE-CA
           IF CA-RETURN-CODE OF WS-DATE-CA > 4
               MOVE XE-SUPPLY-DATE TO XE-PROOF-DEADLINE
           ELSE
               MOVE DC-RESULT-DATE TO XE-PROOF-DEADLINE
           END-IF.
       8200-EXIT.
           EXIT.

       8250-DAYS-TO-DEADLINE SECTION.
       8250-START.
           INITIALIZE WS-DATE-CA
           MOVE "DAYSBTWN" TO CA-FUNCTION OF WS-DATE-CA
           MOVE WS-CO TO CA-COMPANY OF WS-DATE-CA
           MOVE WS-USER TO CA-USER OF WS-DATE-CA
           MOVE XE-AS-AT-DATE TO DC-DATE-1
           MOVE XE-PROOF-DEADLINE TO DC-DATE-2
           CALL "DATECALC" USING WS-DATE-CA
           IF CA-RETURN-CODE OF WS-DATE-CA > 4
               MOVE 0 TO XE-DAYS-TO-DEADLINE
           ELSE
               MOVE DC-RESULT-NUM TO XE-DAYS-TO-DEADLINE
           END-IF.
       8250-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8300-VAT-THAT-FALLS-DUE SECTION.
       8300-START.
           IF WS-RATE = 0
               PERFORM 4050-STANDARD-RATE THRU 4050-EXIT
           END-IF
           IF XE-NET-GBP = 0
               MOVE XE-CUSTOMS-VAL-GBP TO XE-NET-GBP
           END-IF
           COMPUTE WS-AMT2 ROUNDED = XE-NET-GBP * WS-RATE / 100
           MOVE WS-AMT2 TO XE-VAT-DUE-GBP
           MOVE WS-AMT2 TO XE-TOTAL-VAT-DUE.
       8300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8400-UPDATE-EVIDENCE SECTION.
       8400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.Despatch SET ProofOfExportType = ?, "
                  "ProofObtainedDate = CONVERT(date, ?, 112), "
                  "ProofDeadlineDate = CONVERT(date, ?, 112), "
                  "ExportEvidenceStatus = ?, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE DespatchId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XE-PROOF-TYPE TO SQLIO-PARM-VAL(1)
           MOVE XE-PROOF-DATE TO SQLIO-PARM-VAL(2)
           MOVE XE-PROOF-DEADLINE TO SQLIO-PARM-VAL(3)
           MOVE XE-EVIDENCE-STATUS TO SQLIO-PARM-VAL(4)
           MOVE WS-USER TO SQLIO-PARM-VAL(5)
           MOVE XE-DESPATCH-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(6)
           MOVE 6 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       8400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8500-EXEC-UPDATE SECTION.
       8500-START.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
           END-IF
           MOVE 1 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       8500-EXIT.
           EXIT.

       8600-BEGIN SECTION.
       8600-START.
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
           ELSE
               MOVE "Y" TO WS-IN-TXN
           END-IF.
       8600-EXIT.
           EXIT.

       8610-COMMIT SECTION.
       8610-START.
           IF WS-IN-TXN = "Y"
               MOVE "COMT" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
               MOVE "N" TO WS-IN-TXN
           END-IF.
       8610-EXIT.
           EXIT.

       8620-ROLLBACK SECTION.
       8620-START.
           IF WS-IN-TXN = "Y"
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
               MOVE "N" TO WS-IN-TXN
           END-IF.
       8620-EXIT.
           EXIT.

       8650-NO-NULLS SECTION.
       8650-START.
           PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM.
       8650-EXIT.
           EXIT.

       8900-SQL-FAILED SECTION.
       8900-START.
           MOVE "Y" TO WS-ERROR
           MOVE 12 TO WS-RC
           MOVE "SQL0001" TO WS-REASON
           MOVE SQLIO-ERRMSG(1:200) TO WS-MSG
           IF WS-MSG = SPACES
               MOVE "The database refused the request" TO WS-MSG
           END-IF.
       8900-EXIT.
           EXIT.

       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           IF WS-RC < 8
               MOVE 8 TO WS-RC
           END-IF.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "EXP0009" TO WS-REASON
           MOVE SPACES TO WS-MSG
           STRING "Function " FUNCTION TRIM(WS-FUNC)
                  " is not supported by EXPEVID; use CAPTURE, "
                  "RECORDPF , CHECKDUE or STATUS"
               DELIMITED BY SIZE INTO WS-MSG
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9400-REPORT SECTION.
       9400-START.
           PERFORM 8620-ROLLBACK THRU 8620-EXIT
           MOVE WS-RC TO CA-RETURN-CODE OF DFHCOMMAREA
           IF WS-REASON NOT = SPACES
               MOVE WS-REASON TO CA-REASON-CODE OF DFHCOMMAREA
           END-IF
           IF WS-MSG NOT = SPACES
               MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           END-IF
           IF CA-MESSAGE OF DFHCOMMAREA NOT = SPACES
               MOVE 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       9400-EXIT.
           EXIT.
