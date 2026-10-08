      *>================================================================
      *> PROGRAM-ID : PKGRPT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE
      *> PURPOSE    : COMPLIANCE AND PACKAGING DATA REPORT (RPT-019).
      *>              ONE BLOCK PER ITEM SHOWING THE UK VAT
      *>              CLASSIFICATION IN FORCE, THE CUSTOMS DATA
      *>              (COMMODITY CODE, COUNTRY OF ORIGIN, PREFERENCE,
      *>              EXPORT CONTROL, NET WEIGHT), THE PRODUCT
      *>              COMPLIANCE FLAGS (UKCA, DANGEROUS GOODS, UK
      *>              REACH, WEEE, BATTERIES) AND THE PACKAGING
      *>              COMPOSITION BY MATERIAL FOR THE QUANTITY
      *>              DESPATCHED IN THE PERIOD, SO THAT THE PACKAGING
      *>              WEIGHTS RECONCILE TO THE PLASTIC PACKAGING TAX
      *>              RETURN.  MISSING DATA IS LABELLED, NOT ASSUMED.
      *> RUN AS     : PKGRPT CO=STC FROM=yyyymmdd TO=yyyymmdd
      *>              USER=COMPLY1 [ITEM=xxx] [MODE=ALL|DG|FAULTS]
      *> OUTPUT     : run\spool\RPT-019-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, ITMCOMPL, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-01-30 RJH  CR0402 ORIGINAL - VAT LIABILITY LISTING
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS IN THE HEADINGS
      *> 2007-06-01 KMP  CR1760 REVERSE CHARGE SPECIFIED GOODS COLUMN
      *> 2021-01-01 ANW  CR4410 BREXIT - COMMODITY CODE AND ORIGIN
      *> 2022-04-01 ANW  CR4702 PLASTIC PACKAGING TAX WEIGHTS
      *> 2026-10-05 STERP ITM-025 TO ITM-027, REPORT CODE RPT-019
      *>================================================================
      *> REQ: ITM-025, ITM-026, ITM-027
      *> REQ: RPT-019
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PKGRPT.
       AUTHOR. R J HOLLIS.
       DATE-WRITTEN. 30/01/1996.
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
       FD  RPT-FILE REPORT IS R-PKGRPT.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ITMWORK.
       COPY STRPTWS.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY ITMCOMPLCA REPLACING ==DFHCOMMAREA== BY ==WS-CO-CA==.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-ITEM                 PIC X(40) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-FROM                 PIC 9(8) VALUE 0.
           05  WS-TO                   PIC 9(8) VALUE 0.
           05  WS-MODE                 PIC X(10) VALUE SPACES.
       01  WS-RUN.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-LIST-IX              PIC S9(4) VALUE 0.
           05  WS-LIST-CNT             PIC S9(4) VALUE 0.
           05  WS-MX                   PIC S9(4) VALUE 0.
           05  WS-PRINT-IT             PIC X VALUE "Y".
       01  WS-ITEM-TAB.
           05  WS-ITEM-ENT OCCURS 500 TIMES PIC X(40).
       01  WS-DET.
           05  P-ITEM                  PIC X(40) VALUE SPACES.
           05  P-DESC                  PIC X(40) VALUE SPACES.
           05  P-VAT                   PIC X(4) VALUE SPACES.
           05  P-DRC                   PIC X(3) VALUE SPACES.
           05  P-COMM                  PIC X(10) VALUE SPACES.
           05  P-ORIGIN                PIC X(2) VALUE SPACES.
           05  P-PREF                  PIC X(3) VALUE SPACES.
           05  P-NETKG                 PIC S9(9)V9(6) VALUE 0.
           05  P-EXPCTL                PIC X(3) VALUE SPACES.
           05  P-UKCA                  PIC X(3) VALUE SPACES.
           05  P-DG-UN                 PIC X(8) VALUE SPACES.
           05  P-DG-CLASS              PIC X(8) VALUE SPACES.
           05  P-DG-PG                 PIC X(4) VALUE SPACES.
           05  P-REACH                 PIC X(18) VALUE SPACES.
           05  P-WEEE                  PIC X(14) VALUE SPACES.
           05  P-BATTERY               PIC X(14) VALUE SPACES.
           05  P-DESP-QTY              PIC S9(11)V9(3) VALUE 0.
           05  P-PKG-TOT               PIC S9(11)V9(3) VALUE 0.
           05  P-PKG-PLAS              PIC S9(11)V9(3) VALUE 0.
           05  P-PKG-RECY              PIC S9(11)V9(3) VALUE 0.
           05  P-PPT                   PIC X(3) VALUE SPACES.
           05  P-MATERIALS             PIC X(44) VALUE SPACES.
           05  P-NOTE                  PIC X(60) VALUE SPACES.
       01  WS-TOT.
           05  T-ITEMS                 PIC S9(7) VALUE 0.
           05  T-DG                    PIC S9(7) VALUE 0.
           05  T-FAULTS                PIC S9(7) VALUE 0.
           05  T-TOT-G                 PIC S9(13)V9(3) VALUE 0.
           05  T-PLAS-G                PIC S9(13)V9(3) VALUE 0.
           05  T-RECY-G                PIC S9(13)V9(3) VALUE 0.
           05  T-PLAS-KG               PIC S9(11)V9(3) VALUE 0.
           05  T-RECY-KG               PIC S9(11)V9(3) VALUE 0.
       REPORT SECTION.
       RD  R-PKGRPT
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 52
           FOOTING 56.
       COPY STRPTHDR.
       01  RH-COLHEAD TYPE IS REPORT HEADING.
           05  LINE 8.
               10  COLUMN 1    PIC X(45) VALUE
                   "Item, VAT liability, customs data, compliance".
               10  COLUMN 47   PIC X(46) VALUE
                   "flags and packaging composition for the period".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(40) SOURCE P-ITEM.
               10  COLUMN 43   PIC X(40) SOURCE P-DESC.
               10  COLUMN 85   PIC X(4) VALUE "VAT ".
               10  COLUMN 89   PIC X(4) SOURCE P-VAT.
               10  COLUMN 95   PIC X(4) VALUE "DRC ".
               10  COLUMN 99   PIC X(3) SOURCE P-DRC.
               10  COLUMN 104  PIC X(10) VALUE "Commodity ".
               10  COLUMN 114  PIC X(10) SOURCE P-COMM.
               10  COLUMN 126  PIC X(5) VALUE "Org: ".
               10  COLUMN 131  PIC X(2) SOURCE P-ORIGIN.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(9) VALUE "Customs: ".
               10  COLUMN 12   PIC X(5) VALUE "net  ".
               10  COLUMN 17   PIC Z(5)9.999 SOURCE P-NETKG.
               10  COLUMN 29   PIC X(3) VALUE "kg ".
               10  COLUMN 33   PIC X(5) VALUE "pref ".
               10  COLUMN 38   PIC X(3) SOURCE P-PREF.
               10  COLUMN 42   PIC X(7) VALUE "expctl ".
               10  COLUMN 49   PIC X(3) SOURCE P-EXPCTL.
               10  COLUMN 54   PIC X(5) VALUE "UKCA ".
               10  COLUMN 59   PIC X(3) SOURCE P-UKCA.
               10  COLUMN 64   PIC X(3) VALUE "DG ".
               10  COLUMN 67   PIC X(8) SOURCE P-DG-UN.
               10  COLUMN 76   PIC X(8) SOURCE P-DG-CLASS.
               10  COLUMN 85   PIC X(4) SOURCE P-DG-PG.
               10  COLUMN 90   PIC X(18) SOURCE P-REACH.
               10  COLUMN 109  PIC X(14) SOURCE P-WEEE.
               10  COLUMN 124  PIC X(9) SOURCE P-BATTERY.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(11) VALUE "Packaging: ".
               10  COLUMN 14   PIC X(5) VALUE "desp ".
               10  COLUMN 19   PIC Z(8)9.999 SOURCE P-DESP-QTY.
               10  COLUMN 33   PIC X(6) VALUE "total ".
               10  COLUMN 39   PIC Z(8)9.999 SOURCE P-PKG-TOT.
               10  COLUMN 53   PIC X(8) VALUE "plastic ".
               10  COLUMN 61   PIC Z(8)9.999 SOURCE P-PKG-PLAS.
               10  COLUMN 75   PIC X(9) VALUE "recycled ".
               10  COLUMN 84   PIC Z(8)9.999 SOURCE P-PKG-RECY.
               10  COLUMN 98   PIC X(4) VALUE "PPT ".
               10  COLUMN 102  PIC X(3) SOURCE P-PPT.
               10  COLUMN 106  PIC X(27) SOURCE P-MATERIALS.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(60) SOURCE P-NOTE.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(14) VALUE "Items listed:".
               10  COLUMN 16   PIC Z(6)9 SOURCE T-ITEMS.
               10  COLUMN 28   PIC X(18) VALUE "Dangerous goods:".
               10  COLUMN 46   PIC Z(6)9 SOURCE T-DG.
               10  COLUMN 58   PIC X(20) VALUE "Compliance faults:".
               10  COLUMN 78   PIC Z(6)9 SOURCE T-FAULTS.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(20) VALUE "Packaging grammes:".
               10  COLUMN 21   PIC Z(12)9.999 SOURCE T-TOT-G.
               10  COLUMN 39   PIC X(9) VALUE "plastic ".
               10  COLUMN 48   PIC Z(12)9.999 SOURCE T-PLAS-G.
               10  COLUMN 66   PIC X(9) VALUE "recycled".
               10  COLUMN 75   PIC Z(12)9.999 SOURCE T-RECY-G.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(20) VALUE "Plastic kilogrammes:".
               10  COLUMN 21   PIC Z(10)9.999 SOURCE T-PLAS-KG.
               10  COLUMN 39   PIC X(9) VALUE "recycled".
               10  COLUMN 48   PIC Z(10)9.999 SOURCE T-RECY-KG.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(56) VALUE
                   "Packaging weight is the despatched quantity times".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(56) VALUE
                   "the recorded composition per unit (RPT-019).".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-READ-ARGS
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0200-JOB-START
           PERFORM 0300-OPEN-SPOOL
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 1000-SELECT-ITEMS
           PERFORM 1100-REPORT-ITEMS
           PERFORM 0400-CLOSE-SPOOL.
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
           MOVE CM-MODE TO WS-MODE
           MOVE "ITEM" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-ITEM
           END-IF
           IF CM-FROM NOT = SPACES
               COMPUTE WS-FROM = FUNCTION NUMVAL(CM-FROM)
           END-IF
           IF CM-TO NOT = SPACES
               COMPUTE WS-TO = FUNCTION NUMVAL(CM-TO)
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "COMPLY1" TO WS-USER
           END-IF
           IF WS-MODE = SPACES
               MOVE "ALL" TO WS-MODE
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           IF WS-TO = 0
               COMPUTE WS-TO = FUNCTION NUMVAL(WS-NOW(1:8))
           END-IF
           IF WS-FROM = 0
               MOVE WS-TO TO WS-DATE-N
               COMPUTE WS-FROM = (WS-DATE-YYYY * 10000) + 101
           END-IF
           IF WS-FROM > WS-TO
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0101 FROM= is later than TO="
               GO TO 0100-EXIT
           END-IF
           DISPLAY "STERP-I-0100 PKGRPT RPT-019 compliance for "
                   FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "PKGRPT" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG.
       0200-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "RPT-019" TO RP-REPORT-ID
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
           INITIATE R-PKGRPT.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "RPT-019" TO RPT-REPORT-ID
           MOVE "Compliance and packaging data report" TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE WS-TO TO WS-DATE-N
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "Tax point " WS-DATE-DD "/" WS-DATE-MM "/"
                  WS-DATE-YYYY
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           MOVE SPACES TO WS-TEXT
           MOVE WS-FROM TO WS-DATE-N
           STRING "Period " WS-DATE-DD "/" WS-DATE-MM "/"
                  WS-DATE-YYYY " to "
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TO TO WS-DATE-N
           MOVE SPACES TO RPT-SUBTITLE
           STRING FUNCTION TRIM(WS-TEXT) " " WS-DATE-DD "/"
                  WS-DATE-MM "/" WS-DATE-YYYY
               DELIMITED BY SIZE INTO RPT-SUBTITLE
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
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-OK
               MOVE ITM-COL(1) TO RPT-COMPANY-NAME
               MOVE ITM-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       0360-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  THE ITEM LIST IS READ INTO A TABLE FIRST - ITMCOMPL
      *>       OPENS ITS OWN CURSORS AND THE 1996 TEAM WOULD NOT RISK
      *>       A NESTED FETCH ON THE SAME CONNECTION.  OCCURS 500 IS A
      *>       HARD LIMIT; THE OVERFLOW IS REPORTED, NOT IGNORED.
      *>----------------------------------------------------------------
       1000-SELECT-ITEMS SECTION.
       1000-START.
           MOVE 0 TO WS-LIST-CNT
           MOVE SPACES TO SQLIO-STMT
           IF WS-ITEM = SPACES
               STRING "SELECT ItemCode FROM inv.Item WHERE "
                      "CompanyCode = ? AND LifecycleStatus <> "
                      "'RETIRED' ORDER BY ItemCode"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 1 TO SQLIO-PARM-CNT
           ELSE
               STRING "SELECT ItemCode FROM inv.Item WHERE "
                      "CompanyCode = ? AND ItemCode = ? "
                      "ORDER BY ItemCode"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 2 TO SQLIO-PARM-CNT
               MOVE WS-ITEM TO SQLIO-PARM-VAL(2)
               MOVE "N" TO SQLIO-PARM-NULL(2)
           END-IF
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1001 " SQLIO-ERRMSG(1:100)
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA ITM-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   IF WS-LIST-CNT < 500
                       ADD 1 TO WS-LIST-CNT
                       MOVE ITM-COL(1) TO WS-ITEM-ENT(WS-LIST-CNT)
                   ELSE
                       MOVE 4 TO WS-RC
                       MOVE "Y" TO WS-DONE
                       DISPLAY "STERP-W-1002 item table full at 500"
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW.
       1000-EXIT.
           EXIT.

       1100-REPORT-ITEMS SECTION.
       1100-START.
           PERFORM VARYING WS-LIST-IX FROM 1 BY 1
                   UNTIL WS-LIST-IX > WS-LIST-CNT
               MOVE WS-ITEM-ENT(WS-LIST-IX) TO P-ITEM
               PERFORM 1200-ONE-ITEM
           END-PERFORM
           COMPUTE T-PLAS-KG ROUNDED = T-PLAS-G / 1000
           COMPUTE T-RECY-KG ROUNDED = T-RECY-G / 1000.
       1100-EXIT.
           EXIT.

       1200-ONE-ITEM SECTION.
       1200-START.
           MOVE SPACES TO P-DESC P-VAT P-DRC P-COMM P-ORIGIN P-PREF
           MOVE SPACES TO P-EXPCTL P-UKCA P-DG-UN P-DG-CLASS P-DG-PG
           MOVE SPACES TO P-REACH P-WEEE P-BATTERY P-PPT P-MATERIALS
           MOVE SPACES TO P-NOTE
           MOVE 0 TO P-NETKG P-DESP-QTY P-PKG-TOT P-PKG-PLAS
           MOVE 0 TO P-PKG-RECY
           MOVE "Y" TO WS-PRINT-IT
           PERFORM 1300-VAT-CLASS
           PERFORM 1400-CUSTOMS
           PERFORM 1500-COMPLIANCE
           PERFORM 1600-DESPATCHED
           PERFORM 1700-PACKAGING
           IF WS-MODE = "DG" AND P-DG-UN = SPACES
               MOVE "N" TO WS-PRINT-IT
           END-IF
           IF WS-MODE = "FAULTS" AND P-NOTE = SPACES
               MOVE "N" TO WS-PRINT-IT
           END-IF
           IF WS-PRINT-IT = "Y"
               ADD 1 TO T-ITEMS WS-ROWS-OUT
               IF P-DG-UN NOT = SPACES
                   ADD 1 TO T-DG
               END-IF
               ADD P-PKG-TOT TO T-TOT-G
               ADD P-PKG-PLAS TO T-PLAS-G
               ADD P-PKG-RECY TO T-RECY-G
               GENERATE DE-LINE
           END-IF.
       1200-EXIT.
           EXIT.

       1250-RESET-CA SECTION.
       1250-START.
           INITIALIZE WS-CO-CA
           MOVE WS-CO TO CA-COMPANY OF WS-CO-CA
           MOVE WS-USER TO CA-USER OF WS-CO-CA
           MOVE WS-TO TO CA-BUSINESS-DATE OF WS-CO-CA
           MOVE P-ITEM TO CO-ITEM-CODE
           MOVE WS-TO TO CO-TAX-POINT.
       1250-EXIT.
           EXIT.

       1300-VAT-CLASS SECTION.
       1300-START.
           PERFORM 1250-RESET-CA
           MOVE "VATCODE" TO CA-FUNCTION OF WS-CO-CA
           MOVE "SALE" TO CO-DIRECTION
           CALL "ITMCOMPL" USING WS-CO-CA
           IF CA-RETURN-CODE OF WS-CO-CA = 0
               MOVE CO-VAT-CODE TO P-VAT
               IF CO-DRC-GOODS = "Y"
                   MOVE "Yes" TO P-DRC
               ELSE
                   MOVE "No" TO P-DRC
               END-IF
           ELSE
               MOVE "----" TO P-VAT
               MOVE "No VAT classification at that tax point"
                   TO P-NOTE
               ADD 1 TO T-FAULTS
           END-IF.
       1300-EXIT.
           EXIT.

       1400-CUSTOMS SECTION.
       1400-START.
           PERFORM 1250-RESET-CA
           MOVE "CUSTOMS" TO CA-FUNCTION OF WS-CO-CA
           MOVE "SALE" TO CO-DIRECTION
           CALL "ITMCOMPL" USING WS-CO-CA
           IF CA-RETURN-CODE OF WS-CO-CA = 0
               MOVE CO-COMMODITY-CODE TO P-COMM
               MOVE CO-ORIGIN-COUNTRY TO P-ORIGIN
               MOVE CO-NET-WEIGHT-KG TO P-NETKG
               IF CO-PREFERENTIAL = "Y"
                   MOVE "Yes" TO P-PREF
               ELSE
                   MOVE "No" TO P-PREF
               END-IF
               IF CO-EXPORT-CONTROL = "Y"
                   MOVE "Yes" TO P-EXPCTL
               ELSE
                   MOVE "No" TO P-EXPCTL
               END-IF
           ELSE
               MOVE "none held" TO P-COMM
               IF P-NOTE = SPACES
                   MOVE "No customs data - export would be blocked"
                       TO P-NOTE
                   ADD 1 TO T-FAULTS
               END-IF
           END-IF.
       1400-EXIT.
           EXIT.

       1500-COMPLIANCE SECTION.
       1500-START.
           PERFORM 1250-RESET-CA
           MOVE "COMPLY" TO CA-FUNCTION OF WS-CO-CA
           CALL "ITMCOMPL" USING WS-CO-CA
           IF CA-RETURN-CODE OF WS-CO-CA < 8
               IF CO-UKCA-MARKED = "Y"
                   MOVE "Yes" TO P-UKCA
               ELSE
                   MOVE "No" TO P-UKCA
               END-IF
               MOVE CO-DG-UN-NUMBER TO P-DG-UN
               MOVE CO-DG-CLASS TO P-DG-CLASS
               MOVE CO-DG-PACK-GROUP TO P-DG-PG
               MOVE CO-REACH-STATUS TO P-REACH
               MOVE CO-WEEE-CATEGORY TO P-WEEE
               MOVE CO-BATTERY-CATEGORY TO P-BATTERY
           END-IF
           IF CO-FAULT-CNT > 0 AND P-NOTE = SPACES
               MOVE CA-MESSAGE OF WS-CO-CA TO P-NOTE
               ADD 1 TO T-FAULTS
           END-IF.
       1500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1600  THE DESPATCHED QUANTITY FOR THE PERIOD.  THE SIGN ON THE
      *>       MOVEMENT TELLS US WHAT LEFT STOCK, SO NO LIST OF
      *>       MOVEMENT TYPE NAMES HAS TO BE KEPT IN STEP HERE.
      *>----------------------------------------------------------------
       1600-DESPATCHED SECTION.
       1600-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(SUM(CASE WHEN Quantity < 0 THEN "
                  "-Quantity ELSE 0 END),0) FROM inv.StockMovement "
                  "WHERE CompanyCode = ? AND ItemCode = ? AND "
                  "PostingDate >= ? AND PostingDate <= ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE P-ITEM TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE WS-FROM TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE WS-TO TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-OK
               COMPUTE P-DESP-QTY = FUNCTION NUMVAL(ITM-COL(1))
           END-IF.
       1600-EXIT.
           EXIT.

       1700-PACKAGING SECTION.
       1700-START.
           PERFORM 1250-RESET-CA
           MOVE "PACKAGE" TO CA-FUNCTION OF WS-CO-CA
           MOVE P-DESP-QTY TO CO-DESPATCH-QTY
           CALL "ITMCOMPL" USING WS-CO-CA
           IF CA-RETURN-CODE OF WS-CO-CA > 4
               GO TO 1700-EXIT
           END-IF
           MOVE CO-PKG-TOTAL-G TO P-PKG-TOT
           MOVE CO-PKG-PLASTIC-G TO P-PKG-PLAS
           MOVE CO-PKG-RECYCLED-G TO P-PKG-RECY
           IF CO-PPT-LIABLE = "Y"
               MOVE "Yes" TO P-PPT
           ELSE
               MOVE "No" TO P-PPT
           END-IF
           MOVE SPACES TO P-MATERIALS
           MOVE SPACES TO WS-TEXT
           PERFORM VARYING WS-MX FROM 1 BY 1
                   UNTIL WS-MX > CO-LINE-CNT OR WS-MX > 4
               MOVE SPACES TO WS-TEXT2
               STRING FUNCTION TRIM(WS-TEXT)
                      FUNCTION TRIM(CO-L-MATERIAL(WS-MX)) " "
                   DELIMITED BY SIZE INTO WS-TEXT2
               MOVE WS-TEXT2 TO WS-TEXT
           END-PERFORM
           IF CO-LINE-CNT = 0
               MOVE "no packaging recorded" TO P-MATERIALS
           ELSE
               MOVE WS-TEXT TO P-MATERIALS
           END-IF.
       1700-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-PKGRPT
           CLOSE RPT-FILE
           IF T-FAULTS > 0
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
               DISPLAY "STERP-W-0402 " T-FAULTS
                       " item(s) have compliance faults"
           END-IF
           DISPLAY "STERP-I-0403 " WS-ROWS-OUT " item(s) reported"
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0400-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "PKGRPT" TO JL-JOB-NAME
           EVALUATE TRUE
               WHEN WS-RC = 0
                   MOVE "COMPLETE" TO JL-STATUS
               WHEN WS-RC = 4
                   MOVE "WARNING" TO JL-STATUS
               WHEN OTHER
                   MOVE "FAILED" TO JL-STATUS
           END-EVALUATE
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE WS-ROWS-OUT TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       0500-EXIT.
           EXIT.
