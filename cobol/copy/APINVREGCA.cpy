      *>================================================================
      *> APINVREGCA - COMMAREA for APINVREG (supplier invoice and credit
      *>              note register, duplicate control, line coding and
      *>              processing state).
      *> FUNCTIONS  : DUPCHK    exact duplicate and similar-document scan
      *>              VALIDATE  validate header and coded lines only
      *>              REGISTER  validate then write the register entry
      *>              SETSTATE  change registration/owner/block state
      *>              GET       read a registered document back
      *> 1991-04-02 original PURCHASE INVOICE REGISTRATION
      *> 2026-10-05 STERP normalised reference, e-invoice source
      *>================================================================
      *> REQ: AP-003, AP-004, AP-005, AP-008, AP-009, AP-023, AP-031
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  IR-BODY.
               10  IR-SUPPLIER         PIC X(20).
               10  IR-DOC-TYPE         PIC X(12).
                   88  IR-INVOICE                VALUE "INVOICE".
                   88  IR-CREDITNOTE             VALUE "CREDITNOTE".
               10  IR-SUPPLIER-REF     PIC X(40).
               10  IR-REF-NORMALISED   PIC X(40).
               10  IR-REGISTER-NUMBER  PIC X(30).
               10  IR-INVOICE-ID       PIC S9(9) COMP-5.
               10  IR-DATE-RECEIVED    PIC 9(8).
               10  IR-DOCUMENT-DATE    PIC 9(8).
               10  IR-TAX-POINT        PIC 9(8).
               10  IR-GOODS-DATE       PIC 9(8).
               10  IR-POSTING-DATE     PIC 9(8).
               10  IR-DUE-DATE         PIC 9(8).
               10  IR-DISCOUNT-DATE    PIC 9(8).
               10  IR-TERMS-CODE       PIC X(12).
               10  IR-TERM-DAYS        PIC S9(5) SIGN LEADING SEPARATE.
               10  IR-TERMS-OVERRIDE   PIC X(200).
               10  IR-CURRENCY         PIC X(3).
               10  IR-FX-RATE          PIC S9(9)V9(8) COMP-3.
               10  IR-STATED-NET       PIC S9(13)V99 COMP-3.
               10  IR-STATED-VAT       PIC S9(13)V99 COMP-3.
               10  IR-STATED-GROSS     PIC S9(13)V99 COMP-3.
               10  IR-CALC-NET         PIC S9(13)V99 COMP-3.
               10  IR-CALC-VAT         PIC S9(13)V99 COMP-3.
               10  IR-CALC-GROSS       PIC S9(13)V99 COMP-3.
               10  IR-NET-GBP          PIC S9(13)V99 COMP-3.
               10  IR-VAT-GBP          PIC S9(13)V99 COMP-3.
               10  IR-GROSS-GBP        PIC S9(13)V99 COMP-3.
               10  IR-RECOVERABLE-GBP  PIC S9(13)V99 COMP-3.
               10  IR-NONRECOVER-GBP   PIC S9(13)V99 COMP-3.
               10  IR-NET-DIFF         PIC S9(13)V99 COMP-3.
               10  IR-VAT-DIFF         PIC S9(13)V99 COMP-3.
               10  IR-SUPPLIER-VATNO   PIC X(15).
               10  IR-EVIDENCE-TYPE    PIC X(14).
               10  IR-ATTACHMENT-REF   PIC X(200).
               10  IR-EINVOICE-REF     PIC X(60).
               10  IR-SOURCE-CHANNEL   PIC X(10).
                   88  IR-SRC-PAPER              VALUE "PAPER".
                   88  IR-SRC-PDF                VALUE "PDF".
                   88  IR-SRC-EINVOICE           VALUE "EINVOICE".
                   88  IR-SRC-BULK               VALUE "BULK".
               10  IR-OWNER-USER       PIC X(20).
               10  IR-IS-NON-PO        PIC X.
               10  IR-NONPO-REASON     PIC X(60).
               10  IR-REG-STATE        PIC X(14).
                   88  IR-CAPTURED               VALUE "CAPTURED".
                   88  IR-INCOMPLETE             VALUE "INCOMPLETE".
                   88  IR-READY                  VALUE "READY".
                   88  IR-DUPLICATE              VALUE "DUPLICATE".
                   88  IR-REJECTED               VALUE "REJECTED".
               10  IR-MATCH-STATE      PIC X(14).
               10  IR-ACCT-STATE       PIC X(14).
               10  IR-PAY-ELIGIBILITY  PIC X(14).
               10  IR-SETTLE-STATE     PIC X(12).
               10  IR-BLOCK-REASON     PIC X(200).
               10  IR-DISPUTE-REASON   PIC X(200).
               10  IR-RETENTION-FLAG   PIC X.
               10  IR-DUP-FOUND        PIC X.
               10  IR-DUP-REGISTER     PIC X(30).
               10  IR-SIMILAR-CNT      PIC 9(3).
               10  IR-SIMILAR-TABLE.
                   15  IR-SIMILAR OCCURS 10 TIMES.
                       20  IR-S-REGISTER   PIC X(30).
                       20  IR-S-REF        PIC X(40).
                       20  IR-S-DATE       PIC 9(8).
                       20  IR-S-GROSS      PIC S9(13)V99 COMP-3.
                       20  IR-S-REASON     PIC X(40).
               10  IR-LINE-CNT         PIC 9(3).
               10  IR-LINE-TABLE.
                   15  IR-LINE OCCURS 40 TIMES.
                       20  IR-L-NUM        PIC 9(4).
                       20  IR-L-DESC       PIC X(60).
                       20  IR-L-ITEM       PIC X(40).
                       20  IR-L-ORDER      PIC X(30).
                       20  IR-L-ORDER-LINE PIC 9(5).
                       20  IR-L-RECEIPT    PIC X(30).
                       20  IR-L-RECEIPT-LN PIC 9(5).
                       20  IR-L-SERVICE    PIC X(30).
                       20  IR-L-QTY        PIC S9(11)V9(6) COMP-3.
                       20  IR-L-UOM        PIC X(10).
                       20  IR-L-CONV       PIC S9(9)V9(6) COMP-3.
                       20  IR-L-PRICE      PIC S9(11)V9(6) COMP-3.
                       20  IR-L-PRICE-PER  PIC S9(9)V9(6) COMP-3.
                       20  IR-L-CHARGE     PIC S9(11)V99 COMP-3.
                       20  IR-L-NET        PIC S9(11)V99 COMP-3.
                       20  IR-L-VATCODE    PIC X(4).
                       20  IR-L-VATRATE    PIC S9(5)V9(4) COMP-3.
                       20  IR-L-VAT        PIC S9(11)V99 COMP-3.
                       20  IR-L-NET-GBP    PIC S9(11)V99 COMP-3.
                       20  IR-L-VAT-GBP    PIC S9(11)V99 COMP-3.
                       20  IR-L-RECOVER    PIC X.
                       20  IR-L-ACCOUNT    PIC X(12).
                       20  IR-L-COST-CTR   PIC X(20).
                       20  IR-L-PROJECT    PIC X(20).
                       20  IR-L-JOB        PIC X(20).
                       20  IR-L-NATURE     PIC X(10).
                       20  IR-L-STATUS     PIC X(10).
                       20  IR-L-MESSAGE    PIC X(80).
