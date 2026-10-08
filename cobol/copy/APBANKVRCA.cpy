      *>================================================================
      *> APBANKVRCA - COMMAREA for APBANKVR (payee bank detail change,
      *>              Confirmation of Payee, independent call-back and
      *>              approval before use).
      *> FUNCTIONS  : PREPARE   record a new or changed destination
      *>              COP       record the Confirmation of Payee answer
      *>              CALLBACK  record the independent call-back
      *>              APPROVE   request/record the independent approval
      *>              CHECK     may this destination be paid to today?
      *> 1994-07-11 original BACS BENEFICIARY AMENDMENT CONTROL
      *> 2026-10-05 STERP Confirmation of Payee, call-back evidence
      *>================================================================
      *> REQ: AP-002, AP-027, COM-003, COM-008, D-07, D-16
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  BV-BODY.
               10  BV-SUPPLIER         PIC X(20).
               10  BV-BANK-REF         PIC X(10).
               10  BV-BANK-ID          PIC S9(9) COMP-5.
               10  BV-BENEFICIARY      PIC X(120).
               10  BV-COUNTRY          PIC X(2).
               10  BV-CURRENCY         PIC X(3).
               10  BV-SORT-CODE        PIC X(6).
               10  BV-ACCOUNT-NO       PIC X(8).
               10  BV-ROLL-NUMBER      PIC X(18).
               10  BV-IBAN             PIC X(34).
               10  BV-BIC              PIC X(11).
               10  BV-BANK-NAME        PIC X(60).
               10  BV-PAY-METHOD       PIC X(10).
               10  BV-REQUEST-SOURCE   PIC X(30).
                   88  BV-SRC-EMAIL              VALUE "EMAIL".
                   88  BV-SRC-PORTAL             VALUE "PORTAL".
                   88  BV-SRC-LETTER             VALUE "LETTER".
                   88  BV-SRC-CALLBACK           VALUE "CALLBACK".
               10  BV-COP-RESULT       PIC X(12).
                   88  BV-COP-MATCH              VALUE "MATCH".
                   88  BV-COP-CLOSE              VALUE "CLOSEMATCH".
                   88  BV-COP-NOMATCH            VALUE "NOMATCH".
                   88  BV-COP-UNAVAIL            VALUE "UNAVAILABLE".
               10  BV-COP-REFERENCE    PIC X(60).
               10  BV-COP-DECISION     PIC X(200).
               10  BV-CALLBACK-SOURCE  PIC X(120).
               10  BV-CALLBACK-ON-FILE PIC X.
               10  BV-CALLBACK-NOTE    PIC X(300).
               10  BV-CALLBACK-BY      PIC X(20).
               10  BV-PREPARED-BY      PIC X(20).
               10  BV-APPROVER         PIC X(20).
               10  BV-DECISION         PIC X(10).
               10  BV-STATUS           PIC X(12).
                   88  BV-ST-PENDING             VALUE "PENDING".
                   88  BV-ST-VERIFIED            VALUE "VERIFIED".
                   88  BV-ST-APPROVED            VALUE "APPROVED".
                   88  BV-ST-REJECTED            VALUE "REJECTED".
                   88  BV-ST-SUPERSEDED          VALUE "SUPERSEDED".
               10  BV-APPROVAL-ID      PIC S9(9) COMP-5.
               10  BV-SUPERSEDES-ID    PIC S9(9) COMP-5.
               10  BV-CONTENT-HASH     PIC X(64).
               10  BV-USABLE           PIC X.
               10  BV-COP-DONE         PIC X.
               10  BV-CALLBACK-DONE    PIC X.
               10  BV-INVALIDATED-CNT  PIC 9(5).
               10  BV-UNSUBMITTED-CNT  PIC 9(5).
               10  BV-VALID-FROM       PIC 9(8).
               10  BV-NOTE             PIC X(200).
