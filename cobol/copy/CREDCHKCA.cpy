      *>================================================================
      *> CREDCHKCA - COMMAREA FOR CREDCHK, CREDIT EXPOSURE AND RELEASE
      *>   CA-FUNCTION: EXPOSURE  compute exposure only (no decision)
      *>                CHECK     exposure + release decision
      *>                OVERRIDE  request a scoped, expiring override
      *>                APPROVE   approve an override (independent)
      *>                RELEASE   re-test after an override was granted
      *>   exposure = open invoice residuals - eligible AR credits
      *>              + firm unshipped orders + shipped unbilled
      *>              - eligible unapplied deposits      (never below 0)
      *>   CALLED BY : SALES ORDER ENTRY, DESPATCH RELEASE, .NET CLIENT
      *>   REQ: AR-007, AR-008, SO-007, D-06
      *> 1991-04-16 ORIGINAL CREDIT CONTROL PORT (MVS)
      *> 2026-10-05 STERP SHIPPED-UNBILLED AND DEPOSIT LEGS
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  CK-BODY.
               10  CK-CUSTOMER         PIC X(20).
               10  CK-CHECK-POINT      PIC X(10).
                   88  CK-AT-ORDER               VALUE "ORDER".
                   88  CK-AT-AMEND               VALUE "AMEND".
                   88  CK-AT-DESPATCH            VALUE "DESPATCH".
               10  CK-DOCUMENT-REF     PIC X(40).
               10  CK-EVAL-DATE        PIC 9(8).
               10  CK-CURRENCY         PIC X(3).
               10  CK-FX-RATE          PIC S9(9)V9(8) COMP-3.
               10  CK-RATE-DATE        PIC 9(8).
      *>       the candidate commitment, evaluated once (AR-007)
               10  CK-ORDER-VALUE      PIC S9(11)V99 COMP-3.
               10  CK-ORDER-VALUE-GBP  PIC S9(11)V99 COMP-3.
               10  CK-EXCLUDE-REF      PIC X(40).
      *>       the five legs of exposure, all sterling
               10  CK-OPEN-DEBT        PIC S9(11)V99 COMP-3.
               10  CK-OPEN-CREDITS     PIC S9(11)V99 COMP-3.
               10  CK-FIRM-ORDERS      PIC S9(11)V99 COMP-3.
               10  CK-SHIPPED-UNBILL   PIC S9(11)V99 COMP-3.
               10  CK-DEPOSITS         PIC S9(11)V99 COMP-3.
               10  CK-EXPOSURE         PIC S9(11)V99 COMP-3.
               10  CK-LIMIT            PIC S9(11)V99 COMP-3.
               10  CK-HEADROOM         PIC S9(11)V99 COMP-3.
               10  CK-EXCESS           PIC S9(11)V99 COMP-3.
               10  CK-UTILISED-PCT     PIC S9(5)V99 COMP-3.
      *>       overdue control (D-06 hold rule)
               10  CK-OVERDUE-GBP      PIC S9(11)V99 COMP-3.
               10  CK-OVERDUE-THRESH   PIC S9(11)V99 COMP-3.
               10  CK-OVERDUE-DAYS     PIC S9(5).
               10  CK-OVERDUE-TRIGGER  PIC S9(5).
               10  CK-OLDEST-DUE-DATE  PIC 9(8).
               10  CK-DISPUTED-GBP     PIC S9(11)V99 COMP-3.
      *>       account state
               10  CK-HOLD-FLAG        PIC X.
               10  CK-HOLD-REASON      PIC X(200).
               10  CK-PROFORMA-ONLY    PIC X.
               10  CK-ACCOUNT-STATUS   PIC X(10).
               10  CK-SANCTIONS        PIC X(12).
      *>       decision
               10  CK-DECISION         PIC X(10).
                   88  CK-RELEASED               VALUE "RELEASE".
                   88  CK-HELD                   VALUE "HOLD".
                   88  CK-BLOCKED                VALUE "BLOCK".
               10  CK-DECISION-TEXT    PIC X(200).
      *>       override record (AR-008): scope, amount, expiry, authority
               10  CK-OVERRIDE-ID      PIC S9(9) COMP-5.
               10  CK-OVERRIDE-SCOPE   PIC X(12).
               10  CK-OVERRIDE-AMT     PIC S9(11)V99 COMP-3.
               10  CK-OVERRIDE-REASON  PIC X(300).
               10  CK-OVERRIDE-BY      PIC X(20).
               10  CK-OVERRIDE-APPR    PIC X(20).
               10  CK-OVERRIDE-FROM    PIC 9(8).
               10  CK-OVERRIDE-EXPIRY  PIC 9(8).
               10  CK-OVERRIDE-STATUS  PIC X(10).
               10  CK-OVERRIDE-ACTIVE  PIC X.
               10  CK-APPROVER-PERM    PIC X(40).
               10  CK-EXCEPTION-ID     PIC S9(9) COMP-5.
               10  FILLER              PIC X(40).
