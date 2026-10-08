      *>================================================================
      *> APLINWRK - PURCHASE INVOICE LINE WORK AREA
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE  : REQUEST AND ANSWER AREA FOR APINVLIN, THE LINE
      *>            CODING AND VAT ENGINE.  ONE LINE AT A TIME.
      *>              CALL "APINVLIN" USING WS-APLIN
      *> FUNCTIONS: VATCALC  extend, charge, VAT and sterling amounts
      *>            CODECHK  commercial coding and non-PO rules
      *>            BOTH     CODECHK then VATCALC
      *>----------------------------------------------------------------
      *> 1991-04-02 RJH  ORIGINAL
      *> 2021-01-01 AJS  BREXIT - REVERSE CHARGE AND POSTPONED VAT
      *> 2026-10-05 STERP fin.VatCode / fin.VatRate LOOKUP
      *>================================================================
      *> REQ: AP-005, AP-021, AP-025, D-02, D-08
       01  WS-APLIN.
           05  LN-FUNC                 PIC X(8).
           05  LN-COMPANY              PIC X(10).
           05  LN-DATE                 PIC 9(8).
           05  LN-RC                   PIC 9(2).
           05  LN-REASON               PIC X(8).
           05  LN-MESSAGE              PIC X(80).
           05  LN-LINE-NUM             PIC 9(4).
           05  LN-VAT-CODE             PIC X(4).
           05  LN-TAX-POINT            PIC 9(8).
           05  LN-RATE-PCT             PIC S9(5)V9(4).
           05  LN-MECHANISM            PIC X(12).
           05  LN-LIABILITY            PIC X(12).
           05  LN-RECOVERABLE          PIC X.
           05  LN-SELF-ACCOUNTS        PIC X.
           05  LN-VAT-BOX              PIC 9(2).
           05  LN-NET-BOX              PIC 9(2).
           05  LN-QTY                  PIC S9(11)V9(6).
           05  LN-UOM                  PIC X(10).
           05  LN-PRICE                PIC S9(11)V9(6).
           05  LN-PRICE-PER            PIC S9(9)V9(6).
           05  LN-CHARGE               PIC S9(11)V99.
           05  LN-NET                  PIC S9(11)V99.
           05  LN-VAT                  PIC S9(11)V99.
           05  LN-FX-RATE              PIC S9(9)V9(8).
           05  LN-NET-GBP              PIC S9(11)V99.
           05  LN-VAT-GBP              PIC S9(11)V99.
           05  LN-SELF-VAT-GBP         PIC S9(11)V99.
           05  LN-RECOVER-GBP          PIC S9(11)V99.
           05  LN-NONRECOVER-GBP       PIC S9(11)V99.
           05  LN-ACCOUNT              PIC X(12).
           05  LN-COST-CTR             PIC X(20).
           05  LN-PROJECT              PIC X(20).
           05  LN-NATURE               PIC X(10).
           05  LN-ORDER                PIC X(30).
           05  LN-RECEIPT              PIC X(30).
           05  LN-SERVICE              PIC X(30).
           05  LN-ITEM                 PIC X(40).
           05  LN-DESCRIPTION          PIC X(60).
           05  LN-NONPO                PIC X.
           05  LN-NONPO-REASON         PIC X(60).
           05  LN-NONPO-ALLOWED        PIC X(200).
           05  LN-VAT-EVIDENCE         PIC X.
           05  LN-STATUS               PIC X(10).
