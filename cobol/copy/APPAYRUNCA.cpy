      *>================================================================
      *> APPAYRUNCA - COMMAREA for APPAYRUN (payment instructions and
      *>              their outcomes: approved, submitted, accepted,
      *>              settled, rejected, cancelled, uncertain,
      *>              returned).
      *> FUNCTIONS  : CREATE    create instructions from an approved
      *>                        proposal version (beneficiary snapshot)
      *>              SUBMIT    mark submitted to the bank or bureau
      *>              OUTCOME   record the bank's answer
      *>              SETTLE    record definitive settlement
      *>              UNCERTAIN mark an unknown outcome for enquiry
      *>              RETURN    record a returned / recalled payment
      *> 1992-11-16 original BACS SUBMISSION AND RESPONSE LOG
      *> 2026-10-05 STERP uncertain outcome, investigation owner (D-16)
      *>================================================================
      *> REQ: AP-013, AP-016, AP-026, D-16
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  PR-BODY.
               10  PR-PROPOSAL-REF     PIC X(30).
               10  PR-PROPOSAL-ID      PIC S9(9) COMP-5.
               10  PR-VERSION          PIC 9(4).
               10  PR-INSTRUCTION-REF  PIC X(30).
               10  PR-INSTRUCTION-ID   PIC S9(9) COMP-5.
               10  PR-SUPPLIER         PIC X(20).
               10  PR-METHOD           PIC X(10).
               10  PR-BANK-ACCOUNT     PIC X(20).
               10  PR-CURRENCY         PIC X(3).
               10  PR-AMOUNT           PIC S9(13)V99 COMP-3.
               10  PR-AMOUNT-GBP       PIC S9(13)V99 COMP-3.
               10  PR-SETTLED-GBP      PIC S9(13)V99 COMP-3.
               10  PR-FX-RATE          PIC S9(9)V9(8) COMP-3.
               10  PR-VALUE-DATE       PIC 9(8).
               10  PR-BENEFICIARY      PIC X(120).
               10  PR-SORT-CODE        PIC X(6).
               10  PR-ACCOUNT-NO       PIC X(8).
               10  PR-IBAN             PIC X(34).
               10  PR-BIC              PIC X(11).
               10  PR-BANK-ID          PIC S9(9) COMP-5.
               10  PR-COP-SNAPSHOT     PIC X(12).
               10  PR-SCHEME-REF       PIC X(60).
               10  PR-BACS-FILE-REF    PIC X(60).
               10  PR-STATUS           PIC X(12).
                   88  PR-APPROVED               VALUE "APPROVED".
                   88  PR-SUBMITTED              VALUE "SUBMITTED".
                   88  PR-ACCEPTED               VALUE "ACCEPTED".
                   88  PR-SETTLED                VALUE "SETTLED".
                   88  PR-REJECTED               VALUE "REJECTED".
                   88  PR-CANCELLED              VALUE "CANCELLED".
                   88  PR-UNCERTAIN              VALUE "UNCERTAIN".
                   88  PR-RETURNED               VALUE "RETURNED".
               10  PR-NEW-STATUS       PIC X(12).
               10  PR-OUTCOME-NOTE     PIC X(300).
               10  PR-INVEST-OWNER     PIC X(20).
               10  PR-AUTHORISER-1     PIC X(20).
               10  PR-AUTHORISER-2     PIC X(20).
               10  PR-RESERVED-HELD    PIC X.
               10  PR-ALLOW-REPLACE    PIC X.
               10  PR-SETTLE-POSTED    PIC X.
               10  PR-CREATED-CNT      PIC 9(6).
               10  PR-UPDATED-CNT      PIC 9(6).
               10  PR-REMITTANCE-REF   PIC X(30).
               10  PR-REMIT-REISSUE    PIC 9(4).
               10  PR-JOURNAL-NUMBER   PIC X(30).
               10  PR-NOTE             PIC X(200).
