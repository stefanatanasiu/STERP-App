      *>================================================================
      *> APVATEVDCA - COMMAREA for APVATEVD (input VAT evidence rules,
      *>              recoverable and non-recoverable VAT, reverse
      *>              charge, import VAT and the six month unpaid
      *>              input VAT repayment).
      *> FUNCTIONS  : EVIDENCE  decide whether input VAT may be claimed
      *>              SPLIT     recoverable / non-recoverable split
      *>              RCHARGE   reverse charge self-accounting amounts
      *>              SIXMONTH  identify a repayable unpaid invoice
      *>              REPAY     record the repayment in a VAT period
      *>              RECLAIM   reclaim once the invoice is paid
      *> 1991-04-02 original VAT ANALYSIS ON PURCHASE INVOICES
      *> 2021-01-01 BREXIT POSTPONED IMPORT VAT AND C79 EVIDENCE
      *> 2026-10-05 STERP six month repayment (AP-029)
      *>================================================================
      *> REQ: AP-021, AP-025, AP-029, D-02
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  VE-BODY.
               10  VE-SUPPLIER         PIC X(20).
               10  VE-REGISTER-NUMBER  PIC X(30).
               10  VE-INVOICE-ID       PIC S9(9) COMP-5.
               10  VE-SUPPLIER-VATNO   PIC X(15).
               10  VE-VAT-LOCATION     PIC X(3).
               10  VE-EVIDENCE-TYPE    PIC X(14).
                   88  VE-VATINVOICE             VALUE "VATINVOICE".
                   88  VE-SIMPLIFIED             VALUE "SIMPLIFIED".
                   88  VE-C79                    VALUE "C79".
                   88  VE-PVA                    VALUE "PVASTATEMENT".
                   88  VE-SELFBILL               VALUE "SELFBILL".
                   88  VE-REVERSECHG             VALUE "REVERSECHG".
                   88  VE-PROFORMA               VALUE "PROFORMA".
                   88  VE-STATEMENT              VALUE "STATEMENT".
                   88  VE-NONE                   VALUE "NONE".
               10  VE-EVIDENCE-REF     PIC X(60).
               10  VE-VAT-CODE         PIC X(4).
               10  VE-MECHANISM        PIC X(12).
               10  VE-RATE-PCT         PIC S9(5)V9(4) COMP-3.
               10  VE-NET-GBP          PIC S9(13)V99 COMP-3.
               10  VE-CLAIMED-VAT-GBP  PIC S9(13)V99 COMP-3.
               10  VE-ALLOWED-VAT-GBP  PIC S9(13)V99 COMP-3.
               10  VE-HELD-VAT-GBP     PIC S9(13)V99 COMP-3.
               10  VE-NONRECOVER-GBP   PIC S9(13)V99 COMP-3.
               10  VE-OUTPUT-VAT-GBP   PIC S9(13)V99 COMP-3.
               10  VE-INPUT-VAT-GBP    PIC S9(13)V99 COMP-3.
               10  VE-RECOVER-PCT      PIC S9(5)V9(4) COMP-3.
               10  VE-IS-RECOVERABLE   PIC X.
               10  VE-SELF-ACCOUNTS    PIC X.
               10  VE-EVIDENCE-VALID   PIC X.
               10  VE-OUTCOME          PIC X(12).
               10  VE-FAILURE-REASON   PIC X(200).
               10  VE-VAT-BOX          PIC 9(2).
               10  VE-NET-BOX          PIC 9(2).
               10  VE-TAX-POINT        PIC 9(8).
               10  VE-SUPPLY-DATE      PIC 9(8).
               10  VE-DUE-DATE         PIC 9(8).
               10  VE-PAID-DATE        PIC 9(8).
               10  VE-AS-AT-DATE       PIC 9(8).
               10  VE-MONTHS-UNPAID    PIC S9(5) SIGN LEADING SEPARATE.
               10  VE-REPAY-MONTHS     PIC 9(3).
               10  VE-REPAY-DUE-DATE   PIC 9(8).
               10  VE-VAT-PERIOD-KEY   PIC X(10).
               10  VE-REPAY-PERIOD-KEY PIC X(10).
               10  VE-RECLAIM-PERIOD   PIC X(10).
               10  VE-REPAYABLE        PIC X.
               10  VE-ALREADY-REPAID   PIC X.
               10  VE-ALREADY-RECLAIM  PIC X.
               10  VE-EVIDENCE-ID      PIC S9(9) COMP-5.
               10  VE-VAT-TXN-ID       PIC S9(9) COMP-5.
               10  VE-AFFECTED-CNT     PIC 9(6).
               10  VE-NOTE             PIC X(200).
