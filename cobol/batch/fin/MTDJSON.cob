      *>================================================================
      *> PROGRAM-ID : MTDJSON
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER (FIN)
      *> PURPOSE    : MAKING TAX DIGITAL VAT RETURN EXTRACT.  THE NINE
      *>              BOXES ARE PREPARED BY THE ONLINE VATRET9 SERVICE
      *>              AND WRITTEN AS AN HMRC MTD VAT SUBMISSION BODY
      *>              IN JSON TO %STERP_RUN%\mtd.  THE FILE IS NOT
      *>              TRANSMITTED - THE DEMO SYSTEM HAS NO HMRC
      *>              CONNECTION - BUT IT IS THE EXACT PAYLOAD THAT
      *>              WOULD BE SENT, SO IT CAN BE DIFFED AGAINST THE
      *>              FINALISED RETURN.
      *>              FINALISED IS ONLY TRUE WHEN THE VAT RETURN HAS
      *>              ACTUALLY BEEN FINALISED IN THE LEDGER; A DRAFT
      *>              EXTRACT IS ALWAYS MARKED FALSE (FIN-026).
      *>              BOXES 6 TO 9 ARE WHOLE POUNDS, BOXES 1 TO 5 ARE
      *>              POUNDS AND PENCE (FIN-025).
      *> RUN AS     : MTDJSON CO=STC VATPER=26Q3 USER=GLCLERK
      *> OUTPUT     : run\mtd\MTDJSON-<CO>-<stamp>.json
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO, VATRET9
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2019-04-01 KMP  CR2310 ORIGINAL - MAKING TAX DIGITAL MANDATE
      *> 2021-01-01 KMP  CR2455 BREXIT - BOX 2 AND BOX 8/9 WORDING
      *> 2026-10-06 STERP FIN-025/FIN-026 JSON GENERATE EXTRACT
      *>================================================================
      *> REQ: FIN-025, FIN-026
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTDJSON.
       AUTHOR. K M PRESTON.
       DATE-WRITTEN. 01/04/2019.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO WS-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE.
       01  RPT-REC                     PIC X(500).
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY FINBATWK.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY VATRET9CA REPLACING ==DFHCOMMAREA== BY ==WS-VATRET9-CA==
                      LEADING ==CA-== BY ==VH-==.
      *>   the MTD submission body.  The data names are deliberately
      *>   short; JSON GENERATE renames them to the HMRC camel case
       01  MTD-BODY.
           05  M-PERIODKEY             PIC X(8) VALUE SPACES.
           05  M-BOX1                  PIC S9(11)V99 VALUE 0.
           05  M-BOX2                  PIC S9(11)V99 VALUE 0.
           05  M-BOX3                  PIC S9(11)V99 VALUE 0.
           05  M-BOX4                  PIC S9(11)V99 VALUE 0.
           05  M-BOX5                  PIC S9(11)V99 VALUE 0.
           05  M-BOX6                  PIC S9(13) VALUE 0.
           05  M-BOX7                  PIC S9(13) VALUE 0.
           05  M-BOX8                  PIC S9(13) VALUE 0.
           05  M-BOX9                  PIC S9(13) VALUE 0.
           05  M-FINALISED             PIC X(5) VALUE "false".
       01  WS-JSON.
           05  WS-JSON-TEXT            PIC X(2000) VALUE SPACES.
           05  WS-JSON-LEN             PIC S9(9) COMP-5 VALUE 0.
           05  WS-JSON-OK              PIC X VALUE "Y".
       01  WS-OPT.
           05  WS-OBLIG                PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           MOVE "MTDJSON" TO RPT-REPORT-ID
           PERFORM 9100-JOB-START THRU 9100-EXIT
           PERFORM 1000-PREPARE THRU 1000-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 2000-BUILD-JSON THRU 2000-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 3000-WRITE-FILE THRU 3000-EXIT.
       0000-FINISH.
           PERFORM 9150-JOB-END THRU 9150-EXIT
           MOVE WS-RC TO RETURN-CODE
           STOP RUN.

       0100-READ-ARGS.
           PERFORM 9500-STD-ARGS THRU 9500-EXIT
           MOVE "VATPER" TO FB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           MOVE FB-TEXT(1:10) TO WS-PERIOD-KEY
           IF WS-PERIOD-KEY = SPACES
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0101 VATPER=<period key> is required"
               GO TO 0100-EXIT
           END-IF
           DISPLAY "STERP-I-0100 MTDJSON VAT extract for "
                   FUNCTION TRIM(WS-CO) " period "
                   FUNCTION TRIM(WS-PERIOD-KEY).
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-025  the nine boxes come from the one service that
      *> knows the rules - this program never re-adds VAT itself
      *>----------------------------------------------------------------
       1000-PREPARE.
           INITIALIZE WS-VATRET9-CA
           MOVE "PREPARE" TO VH-FUNCTION
           MOVE WS-CO TO VH-COMPANY
           MOVE WS-USER TO VH-USER
           MOVE WS-ASAT TO VH-BUSINESS-DATE
           MOVE WS-PERIOD-KEY TO VR-VAT-PERIOD-KEY
           CALL "VATRET9" USING WS-VATRET9-CA
           IF VH-RETURN-CODE > 4
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-1001 " VH-MESSAGE
               GO TO 1000-EXIT
           END-IF
           MOVE WS-PERIOD-KEY TO M-PERIODKEY
           MOVE VR-BOX1 TO M-BOX1
           MOVE VR-BOX2 TO M-BOX2
           MOVE VR-BOX3 TO M-BOX3
           MOVE VR-BOX4 TO M-BOX4
           MOVE VR-BOX5 TO M-BOX5
           MOVE VR-BOX6 TO M-BOX6
           MOVE VR-BOX7 TO M-BOX7
           MOVE VR-BOX8 TO M-BOX8
           MOVE VR-BOX9 TO M-BOX9
           MOVE "false" TO M-FINALISED
           IF VR-RETURN-STATUS = "FINALISED" OR
              VR-RETURN-STATUS = "SUBMITTED"
               MOVE "true" TO M-FINALISED
           END-IF
      *>   Box 3 = Box 1 + Box 2 and Box 5 = Box 3 - Box 4 are the
      *>   HMRC identities; the extract refuses to write a payload
      *>   that would fail them at the gateway
           COMPUTE FB-AMT = M-BOX1 + M-BOX2 - M-BOX3
           COMPUTE FB-AMT2 = M-BOX3 - M-BOX4 - M-BOX5
           IF FB-AMT > 0.004 OR FB-AMT < -0.004 OR
              FB-AMT2 > 0.004 OR FB-AMT2 < -0.004
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-1002 box identities do not hold - "
                       "the return is not submittable"
               GO TO 1000-EXIT
           END-IF
           ADD 1 TO WS-ROWS-OUT
           DISPLAY "STERP-I-1003 nine boxes prepared, "
                   VR-RECORD-CNT " VAT record(s), "
                   VR-LATE-CNT " late entry(ies)".
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-026  the payload uses the HMRC field names exactly
      *>----------------------------------------------------------------
       2000-BUILD-JSON.
           MOVE SPACES TO WS-JSON-TEXT
           JSON GENERATE WS-JSON-TEXT FROM MTD-BODY
                COUNT IN WS-JSON-LEN
                NAME OF MTD-BODY IS OMITTED
                     M-PERIODKEY IS "periodKey"
                     M-BOX1 IS "vatDueSales"
                     M-BOX2 IS "vatDueAcquisitions"
                     M-BOX3 IS "totalVatDue"
                     M-BOX4 IS "vatReclaimedCurrPeriod"
                     M-BOX5 IS "netVatDue"
                     M-BOX6 IS "totalValueSalesExVAT"
                     M-BOX7 IS "totalValuePurchasesExVAT"
                     M-BOX8 IS "totalValueGoodsSuppliedExVAT"
                     M-BOX9 IS "totalAcquisitionsExVAT"
                     M-FINALISED IS "finalised"
                ON EXCEPTION
                     MOVE "N" TO WS-JSON-OK
           END-JSON
           IF WS-JSON-OK NOT = "Y"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-2001 the JSON payload could not be "
                       "generated"
               GO TO 2000-EXIT
           END-IF
      *>   finalised must be a JSON boolean, not the string "true" -
      *>   the quotes around the value are stripped here rather than
      *>   complicating the record layout
           PERFORM 2100-UNQUOTE THRU 2100-EXIT.
       2000-EXIT.
           EXIT.

       2100-UNQUOTE.
           PERFORM VARYING FB-I FROM 1 BY 1
                   UNTIL FB-I > WS-JSON-LEN - 6
               IF WS-JSON-TEXT(FB-I:7) = ":""true"""
                   MOVE ":true  " TO WS-JSON-TEXT(FB-I:7)
               END-IF
               IF WS-JSON-TEXT(FB-I:8) = ":""false"""
                   MOVE ":false  " TO WS-JSON-TEXT(FB-I:8)
               END-IF
           END-PERFORM.
       2100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       3000-WRITE-FILE.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "MTD" TO RP-FOLDER
           MOVE "MTDJSON" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "json" TO RP-EXTENSION
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-3001 cannot build the mtd path"
               GO TO 3000-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-3002 cannot open "
                       FUNCTION TRIM(WS-SPOOL-NAME)
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO RPT-REC
           MOVE WS-JSON-TEXT(1:500) TO RPT-REC
           WRITE RPT-REC
           IF WS-JSON-LEN > 500
               MOVE SPACES TO RPT-REC
               MOVE WS-JSON-TEXT(501:500) TO RPT-REC
               WRITE RPT-REC
           END-IF
           CLOSE RPT-FILE
           DISPLAY "STERP-I-3003 " WS-JSON-LEN " byte(s) written"
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       3000-EXIT.
           EXIT.

       COPY FINBATPR.
