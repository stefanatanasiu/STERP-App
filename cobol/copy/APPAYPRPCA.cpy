      *>================================================================
      *> APPAYPRPCA - COMMAREA for APPAYPRP (payment proposal selection,
      *>              usable credits, exclusions and reservation against
      *>              duplicate selection).
      *> FUNCTIONS  : SELECT    build a proposal (preview, no reserve)
      *>              RESERVE   build and reserve the selected amounts
      *>              RELEASE   release the reservation of a proposal
      *>              TOTALS    recalculate the cash requirement
      *>              METHOD    payment method / processing day routing
      *> 1992-11-16 original BACS PAYMENT SELECTION RUN
      *> 2026-10-05 STERP reservation proc, CHAPS / FPS routing (D-16)
      *>================================================================
      *> REQ: AP-011, AP-012, AP-020, AP-026, D-16
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  PP-BODY.
               10  PP-PROPOSAL-REF     PIC X(30).
               10  PP-PROPOSAL-ID      PIC S9(9) COMP-5.
               10  PP-VERSION          PIC 9(4).
               10  PP-CUTOFF-DATE      PIC 9(8).
               10  PP-PAY-DATE         PIC 9(8).
               10  PP-PAY-DATE-RAW     PIC 9(8).
               10  PP-BACS-INPUT-DATE  PIC 9(8).
               10  PP-BACS-PROC-DATE   PIC 9(8).
               10  PP-METHOD           PIC X(10).
                   88  PP-BACS                   VALUE "BACS".
                   88  PP-FPS                    VALUE "FPS".
                   88  PP-CHAPS                  VALUE "CHAPS".
                   88  PP-SWIFT                  VALUE "SWIFT".
                   88  PP-SEPA                   VALUE "SEPA".
               10  PP-BANK-ACCOUNT     PIC X(20).
               10  PP-CURRENCY         PIC X(3).
               10  PP-SUPPLIER-FROM    PIC X(20).
               10  PP-SUPPLIER-TO      PIC X(20).
               10  PP-MIN-PAYMENT      PIC S9(11)V99 COMP-3.
               10  PP-FUNDS-LIMIT      PIC S9(13)V99 COMP-3.
               10  PP-SINGLE-LIMIT     PIC S9(13)V99 COMP-3.
               10  PP-CHAPS-THRESHOLD  PIC S9(13)V99 COMP-3.
               10  PP-CASH-PRINCIPAL   PIC S9(13)V99 COMP-3.
               10  PP-CREDITS-APPLIED  PIC S9(13)V99 COMP-3.
               10  PP-DISCOUNTS-TAKEN  PIC S9(13)V99 COMP-3.
               10  PP-FEES             PIC S9(13)V99 COMP-3.
               10  PP-TOTAL-OUTFLOW    PIC S9(13)V99 COMP-3.
               10  PP-RESERVED-TOTAL   PIC S9(13)V99 COMP-3.
               10  PP-SELECTED-CNT     PIC 9(6).
               10  PP-EXCLUDED-CNT     PIC 9(6).
               10  PP-SUPPLIER-CNT     PIC 9(6).
               10  PP-STATUS           PIC X(12).
               10  PP-CONTENT-HASH     PIC X(64).
               10  PP-IS-PROC-DAY      PIC X.
               10  PP-DAY-ADJUSTED     PIC X.
               10  PP-METHOD-CHANGED   PIC X.
               10  PP-OVER-LIMIT       PIC X.
               10  PP-PREPARED-BY      PIC X(20).
               10  PP-MODE             PIC X(10).
               10  PP-LINE-CNT         PIC 9(4).
               10  PP-LINE-TABLE.
                   15  PP-LINE OCCURS 60 TIMES.
                       20  PP-L-NUM        PIC 9(5).
                       20  PP-L-SUPPLIER   PIC X(20).
                       20  PP-L-ITEM-ID    PIC S9(9) COMP-5.
                       20  PP-L-DOCUMENT   PIC X(30).
                       20  PP-L-TYPE       PIC X(12).
                       20  PP-L-DUE-DATE   PIC 9(8).
                       20  PP-L-CURRENCY   PIC X(3).
                       20  PP-L-GROSS      PIC S9(13)V99 COMP-3.
                       20  PP-L-GROSS-GBP  PIC S9(13)V99 COMP-3.
                       20  PP-L-DISCOUNT   PIC S9(11)V99 COMP-3.
                       20  PP-L-CREDIT     PIC S9(11)V99 COMP-3.
                       20  PP-L-RESERVED   PIC S9(13)V99 COMP-3.
                       20  PP-L-NET-PAY    PIC S9(13)V99 COMP-3.
                       20  PP-L-BANK-ID    PIC S9(9) COMP-5.
                       20  PP-L-EXCLUDED   PIC X.
                       20  PP-L-REASON     PIC X(60).
               10  PP-NOTE             PIC X(200).
