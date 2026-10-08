      *>================================================================
      *> LATEINTCA - COMMAREA FOR LATEINT, LATE PAYMENT OF COMMERCIAL
      *>             DEBTS (INTEREST) ACT 1998
      *>   CA-FUNCTION: CALC      statutory interest and compensation
      *>                CLAIM     record the claim for approval
      *>                APPROVE   credit manager approval (D-28)
      *>                WAIVE     waive with a mandatory reason
      *>                ISSUE     raise the claim invoice (no VAT)
      *>   interest = principal x (reference rate + 8%) x days / 365
      *>   compensation = 40 / 70 / 100 by debt band
      *>   reference rate = BoE base at 31 December (Jan-Jun) or
      *>                    30 June (Jul-Dec), effective dated
      *>   REQ: AR-024, D-25, D-28
      *> 1999-04-06 RJH CR1120 LATE PAYMENT ACT 1998 PHASE 1
      *> 2002-08-07 RJH CR1455 ACT EXTENDED TO ALL BUSINESS DEBTS
      *> 2013-03-16 KMP CR2001 FIXED SUM COMPENSATION 40/70/100
      *> 2026-10-05 STERP EFFECTIVE DATED REFERENCE RATE
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  LI-BODY.
               10  LI-CUSTOMER         PIC X(20).
               10  LI-INVOICE-NO       PIC X(30).
               10  LI-OPEN-ITEM-ID     PIC S9(9) COMP-5.
               10  LI-DUE-DATE         PIC 9(8).
               10  LI-CALC-TO-DATE     PIC 9(8).
               10  LI-PRINCIPAL        PIC S9(11)V99 COMP-3.
               10  LI-DAYS-OVERDUE     PIC S9(5).
               10  LI-QUALIFYING       PIC X.
               10  LI-ALT-REMEDY       PIC X.
               10  LI-REF-RATE-PCT     PIC S9(3)V9(4) COMP-3.
               10  LI-RATE-BASIS-DATE  PIC 9(8).
               10  LI-RATE-SOURCE      PIC X(20).
               10  LI-MARGIN-PCT       PIC S9(3)V9(4) COMP-3.
               10  LI-TOTAL-RATE-PCT   PIC S9(3)V9(4) COMP-3.
               10  LI-INTEREST         PIC S9(11)V99 COMP-3.
               10  LI-COMPENSATION     PIC S9(11)V99 COMP-3.
               10  LI-COMP-BAND        PIC 9(1).
               10  LI-RECOVERY-COSTS   PIC S9(11)V99 COMP-3.
               10  LI-RECOVERY-EVID    PIC X(200).
               10  LI-TOTAL-CLAIM      PIC S9(11)V99 COMP-3.
               10  LI-VAT-BEARING      PIC X.
      *>       claim lifecycle
               10  LI-CLAIM-REF        PIC X(30).
               10  LI-CLAIM-ID         PIC S9(9) COMP-5.
               10  LI-STATUS           PIC X(10).
               10  LI-APPROVER         PIC X(20).
               10  LI-WAIVER-REASON    PIC X(300).
               10  LI-CLAIM-INVOICE    PIC X(30).
               10  LI-JOURNAL-NO       PIC X(30).
               10  LI-EXPLANATION      PIC X(200).
               10  FILLER              PIC X(40).
