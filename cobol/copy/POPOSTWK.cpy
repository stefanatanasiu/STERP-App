      *>================================================================
      *> POPOSTWK - PURCHASING NOMINAL POSTING WORK AREA
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE  : REQUEST AREA FOR POPOST, THE ONLY ROUTE FROM THE
      *>            PURCHASING SUITE INTO THE NOMINAL LEDGER.  IT
      *>            CARRIES ITS OWN HEADER RATHER THAN STCAHDR SO A
      *>            PURCHASING PROGRAM CAN HOLD ITS OWN COMMAREA AND
      *>            THIS AREA AT THE SAME TIME.
      *>              CALL "POPOST" USING WS-POPOST
      *> FUNCTIONS: STAGE  stage the lines only
      *>            POST   stage then post via fin.usp_PostJournal
      *>            CLEAR  discard staged lines for a correlation id
      *> EVENTS   : PO.RECEIPT        goods received, GRNI credited
      *>            PO.SERVICEACCEPT  service accepted, accrual
      *>            PO.RETURN         supplier return clears GRNI
      *>            PO.LANDEDEST      estimated landed cost accrual
      *>            PO.LANDEDACT      actual landed cost true up
      *>            PO.IMPORTDUTY     customs duty into landed cost
      *>            PO.IMPORTVAT      postponed import VAT both sides
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-04-02 RJH  CR0788 DISTRIBUTION SUITE NOMINAL INTERFACE
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS ON POSTING DATES
      *> 2021-01-01 TMW  BREXIT - POSTPONED IMPORT VAT LEGS
      *> 2026-10-06 STERP fin.JournalStage + fin.usp_PostJournal
      *>================================================================
      *> REQ: PO-009, PO-014, PO-017, COM-014, COM-015, FIN-004
       01  WS-POPOST.
           05  PP-FUNC                 PIC X(8).
           05  PP-COMPANY              PIC X(10).
           05  PP-SITE                 PIC X(10).
           05  PP-USER                 PIC X(20).
           05  PP-BUSINESS-DATE        PIC 9(8).
           05  PP-CORRELATION          PIC X(64).
           05  PP-RC                   PIC 9(2).
           05  PP-REASON               PIC X(8).
           05  PP-MESSAGE              PIC X(200).
           05  PP-EVENT-CODE           PIC X(30).
           05  PP-JOURNAL-TYPE         PIC X(12).
           05  PP-SOURCE-REF           PIC X(40).
           05  PP-DESCRIPTION          PIC X(200).
           05  PP-POSTING-DATE         PIC 9(8).
           05  PP-DOCUMENT-DATE        PIC 9(8).
           05  PP-CURRENCY             PIC X(3).
           05  PP-FX-RATE              PIC S9(9)V9(8).
           05  PP-SERIES-CODE          PIC X(20).
           05  PP-EVIDENCE-REF         PIC X(120).
           05  PP-SUBLEDGER-TYPE       PIC X(6).
           05  PP-SUBLEDGER-REF        PIC X(40).
           05  PP-LINE-CNT             PIC 9(3).
           05  PP-LINE-TABLE.
               10  PP-LINE OCCURS 30 TIMES.
                   15  PP-L-ACCOUNT    PIC X(12).
                   15  PP-L-DR         PIC S9(13)V99.
                   15  PP-L-CR         PIC S9(13)V99.
                   15  PP-L-COST-CTR   PIC X(20).
                   15  PP-L-PROJECT    PIC X(20).
                   15  PP-L-VATCODE    PIC X(4).
                   15  PP-L-JOB        PIC X(20).
                   15  PP-L-NARRATIVE  PIC X(60).
           05  PP-JOURNAL-NUMBER       PIC X(30).
           05  PP-JOURNAL-ID           PIC S9(9).
           05  PP-FISCAL-YEAR          PIC 9(4).
           05  PP-PERIOD-NO            PIC 9(2).
           05  PP-TOTAL-DR-GBP         PIC S9(13)V99.
           05  PP-TOTAL-CR-GBP         PIC S9(13)V99.
           05  PP-WAS-DUPLICATE        PIC X.
           05  PP-BALANCED             PIC X.
