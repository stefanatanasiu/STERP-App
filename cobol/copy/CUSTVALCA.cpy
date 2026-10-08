      *>================================================================
      *> CUSTVALCA - COMMAREA FOR CUSTVAL, SALES LEDGER ACCOUNT CONTROL
      *>   CA-FUNCTION: VALIDATE  check a proposed account is complete
      *>                ACTIVATE  move PROSPECT/SUSPENDED to ACTIVE
      *>                SUSPEND   suspend with a reason
      *>                RETIRE    retire (outstanding items stay payable)
      *>                RELATE    record an approved party relationship
      *>                SCREEN    external verification and sanctions
      *>   REQ: AR-001, AR-002, AR-028, COM-029
      *> 1991-04-16 ORIGINAL SALES LEDGER PORT (MVS)
      *> 2026-10-05 STERP COMPANIES HOUSE / HMRC / SANCTIONS EVIDENCE
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  CV-BODY.
               10  CV-CUSTOMER         PIC X(20).
               10  CV-LEGAL-NAME       PIC X(120).
               10  CV-TRADING-NAME     PIC X(120).
               10  CV-LEGAL-FORM       PIC X(14).
                   88  CV-INDIVIDUAL   VALUE "SOLETRADER"
                                             "PARTNERSHIP".
               10  CV-CH-NUMBER        PIC X(10).
               10  CV-CH-STATUS        PIC X(20).
               10  CV-VAT-LOCATION     PIC X(3).
               10  CV-VAT-REG-NO       PIC X(15).
               10  CV-VAT-VALIDATED    PIC X.
               10  CV-VAT-VALID-REF    PIC X(40).
               10  CV-VAT-EXCEPT-REF   PIC X(40).
               10  CV-CURRENCY         PIC X(3).
               10  CV-TERMS-CODE       PIC X(12).
               10  CV-TERM-DAYS        PIC S9(5).
               10  CV-COLLECT-METHOD   PIC X(10).
               10  CV-CONTROL-ACCOUNT  PIC X(12).
               10  CV-STATUS           PIC X(10).
               10  CV-NEW-STATUS       PIC X(10).
               10  CV-STATUS-REASON    PIC X(200).
               10  CV-CREDIT-LIMIT     PIC S9(11)V99 COMP-3.
               10  CV-PERSONAL-DATA    PIC X.
               10  CV-HAS-BILLING      PIC X.
               10  CV-HAS-REGISTERED   PIC X.
      *>       party relationships - AR-002, each role explicit
               10  CV-RELATION-ROLE    PIC X(12).
                   88  CV-ROLE-INVOICE VALUE "INVOICE".
                   88  CV-ROLE-PAYER   VALUE "PAYER".
                   88  CV-ROLE-STATE   VALUE "STATEMENT".
               10  CV-RELATION-CUST    PIC X(20).
               10  CV-RELATION-OK      PIC X.
               10  CV-INVOICE-CUST     PIC X(20).
               10  CV-PAYER-CUST       PIC X(20).
               10  CV-STATEMENT-CUST   PIC X(20).
      *>       external verification - AR-028 / COM-029
               10  CV-SANCTIONS-STATUS PIC X(12).
               10  CV-SANCTIONS-HITS   PIC S9(9) COMP-5.
               10  CV-SCREEN-ID        PIC S9(9) COMP-5.
               10  CV-CREDIT-AGENCY    PIC X(40).
               10  CV-CREDIT-SCORE     PIC S9(9) COMP-5.
               10  CV-CREDIT-SCORE-DT  PIC 9(8).
               10  CV-REVIEW-DATE      PIC 9(8).
               10  CV-EXCEPTION-ID     PIC S9(9) COMP-5.
      *>       outcome of VALIDATE - the missing mandatory items
               10  CV-MISSING-CNT      PIC 9(2).
               10  CV-MISSING-TAB.
                   15  CV-MISSING      OCCURS 12 TIMES
                                       PIC X(40).
               10  CV-DECISION         PIC X(10).
                   88  CV-DEC-OK                 VALUE "OK".
                   88  CV-DEC-INCOMPLETE         VALUE "INCOMPLETE".
                   88  CV-DEC-BLOCKED            VALUE "BLOCKED".
               10  FILLER              PIC X(40).
