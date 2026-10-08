      *>================================================================
      *> APPOSTWK - PURCHASE LEDGER NOMINAL POSTING WORK AREA
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE  : REQUEST AREA FOR APPOST, THE PURCHASE LEDGER'S
      *>            ONLY ROUTE INTO THE NOMINAL LEDGER.  IT CARRIES
      *>            ITS OWN HEADER RATHER THAN STCAHDR SO THAT A
      *>            LEDGER PROGRAM CAN HOLD ITS OWN COMMAREA AND THIS
      *>            AREA AT THE SAME TIME.
      *>              CALL "APPOST" USING WS-APPOST
      *> FUNCTIONS: STAGE  stage the lines only
      *>            POST   stage then post through fin.usp_PostJournal
      *>            CLEAR  discard staged lines for a correlation id
      *>----------------------------------------------------------------
      *> 1991-02-04 RJH  ORIGINAL NOMINAL LEDGER INTERFACE
      *> 2026-10-05 STERP fin.JournalStage + fin.usp_PostJournal
      *>================================================================
      *> REQ: AP-021, AP-022, COM-014, COM-015, FIN-004
       01  WS-APPOST.
           05  PJ-FUNC                 PIC X(8).
           05  PJ-COMPANY              PIC X(10).
           05  PJ-SITE                 PIC X(10).
           05  PJ-USER                 PIC X(20).
           05  PJ-BUSINESS-DATE        PIC 9(8).
           05  PJ-CORRELATION          PIC X(64).
           05  PJ-RC                   PIC 9(2).
           05  PJ-REASON               PIC X(8).
           05  PJ-MESSAGE              PIC X(200).
           05  PJ-EVENT-CODE           PIC X(30).
           05  PJ-JOURNAL-TYPE         PIC X(12).
           05  PJ-SOURCE-REF           PIC X(40).
           05  PJ-DESCRIPTION          PIC X(200).
           05  PJ-POSTING-DATE         PIC 9(8).
           05  PJ-DOCUMENT-DATE        PIC 9(8).
           05  PJ-CURRENCY             PIC X(3).
           05  PJ-FX-RATE              PIC S9(9)V9(8).
           05  PJ-SERIES-CODE          PIC X(20).
           05  PJ-EVIDENCE-REF         PIC X(120).
           05  PJ-SUBLEDGER-TYPE       PIC X(6).
           05  PJ-SUBLEDGER-REF        PIC X(40).
           05  PJ-LINE-CNT             PIC 9(3).
           05  PJ-LINE-TABLE.
               10  PJ-LINE OCCURS 30 TIMES.
                   15  PJ-L-ACCOUNT    PIC X(12).
                   15  PJ-L-DR         PIC S9(13)V99.
                   15  PJ-L-CR         PIC S9(13)V99.
                   15  PJ-L-COST-CTR   PIC X(20).
                   15  PJ-L-PROJECT    PIC X(20).
                   15  PJ-L-VATCODE    PIC X(4).
                   15  PJ-L-JOB        PIC X(20).
                   15  PJ-L-BANK       PIC X(20).
                   15  PJ-L-NARRATIVE  PIC X(60).
           05  PJ-JOURNAL-NUMBER       PIC X(30).
           05  PJ-JOURNAL-ID           PIC S9(9).
           05  PJ-FISCAL-YEAR          PIC 9(4).
           05  PJ-PERIOD-NO            PIC 9(2).
           05  PJ-TOTAL-DR-GBP         PIC S9(13)V99.
           05  PJ-TOTAL-CR-GBP         PIC S9(13)V99.
           05  PJ-WAS-DUPLICATE        PIC X.
           05  PJ-BALANCED             PIC X.
