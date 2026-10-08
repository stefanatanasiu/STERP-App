      *>================================================================
      *> APSUPVALCA - COMMAREA for APSUPVAL (supplier lifecycle, holds,
      *>              statutory payment term limits and payability).
      *> FUNCTIONS  : VALIDATE  read and validate a supplier account
      *>              HOLD      set a purchasing and/or payment hold
      *>              RELEASE   lift a hold with a recorded reason
      *>              TERMCHK   check agreed terms against the D-25 limit
      *>              CANPAY    may this account be paid today?
      *> 1991-03-18 original PURCHASE LEDGER SUPPLIER VALIDATION
      *> 2026-10-05 STERP business size, sanctions, D-25 term limit
      *>================================================================
      *> REQ: AP-001, AP-024, AP-030, D-25, D-29
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  SV-BODY.
               10  SV-SUPPLIER         PIC X(20).
               10  SV-HOLD-TYPE        PIC X(10).
                   88  SV-HOLD-PURCH             VALUE "PURCHASE".
                   88  SV-HOLD-PAY               VALUE "PAYMENT".
                   88  SV-HOLD-BOTH              VALUE "BOTH".
               10  SV-REASON           PIC X(200).
               10  SV-LEGAL-NAME       PIC X(120).
               10  SV-TRADING-NAME     PIC X(120).
               10  SV-LEGAL-FORM       PIC X(14).
               10  SV-STATUS           PIC X(10).
                   88  SV-ST-ACTIVE              VALUE "ACTIVE".
                   88  SV-ST-PENDING             VALUE "PENDING".
                   88  SV-ST-ONHOLD              VALUE "ONHOLD".
                   88  SV-ST-SUSPENDED           VALUE "SUSPENDED".
                   88  SV-ST-RETIRED             VALUE "RETIRED".
               10  SV-BUSINESS-SIZE    PIC X(8).
                   88  SV-SMALL                  VALUE "SMALL".
               10  SV-CURRENCY         PIC X(3).
               10  SV-ALLOWED-CCY      PIC X(40).
               10  SV-PAY-METHOD       PIC X(10).
               10  SV-VAT-LOCATION     PIC X(3).
               10  SV-VAT-REG-NO       PIC X(15).
               10  SV-COUNTRY          PIC X(2).
               10  SV-SANCTIONS        PIC X(12).
               10  SV-CIS-FLAG         PIC X.
               10  SV-EINVOICE-FLAG    PIC X.
               10  SV-AP-CONTROL       PIC X(12).
               10  SV-EXPENSE-ACCOUNT  PIC X(12).
               10  SV-DIM-COST-CENTRE  PIC X(20).
               10  SV-OWNER-USER       PIC X(20).
               10  SV-BUYER-USER       PIC X(20).
               10  SV-TERMS-CODE       PIC X(12).
               10  SV-TERM-DAYS        PIC S9(5) SIGN LEADING SEPARATE.
               10  SV-MAX-TERM-DAYS    PIC S9(5) SIGN LEADING SEPARATE.
               10  SV-MIN-PAYMENT      PIC S9(11)V99 COMP-3.
               10  SV-OPEN-BAL-GBP     PIC S9(13)V99 COMP-3.
               10  SV-OPEN-ITEM-CNT    PIC 9(6).
               10  SV-PURCHASE-HOLD    PIC X.
               10  SV-PAYMENT-HOLD     PIC X.
               10  SV-CAN-PURCHASE     PIC X.
               10  SV-CAN-PAY          PIC X.
               10  SV-TERM-EXCEPTION   PIC X.
               10  SV-DD-OUTSTANDING   PIC 9(3).
               10  SV-DD-FAILED        PIC 9(3).
               10  SV-NOTE             PIC X(200).
