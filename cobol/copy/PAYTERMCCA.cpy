      *>================================================================
      *> PAYTERMCCA - COMMAREA for PAYTERMC, the purchase payment
      *>              terms policy check.
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> PO-027 and D-25.  Two limits apply and the TIGHTER one wins:
      *>   the company's own payment policy (core.Parameter
      *>   PO.TERMS.POLICYDAYS, baseline 30 days), and
      *>   the statutory maximum for the supplier's size (baseline
      *>   60 days from receipt of a valid invoice for ANY supplier,
      *>   PO.TERMS.STATUTORYMAX; a small or medium supplier may have
      *>   a tighter figure through PO.TERMS.SMEMAX).
      *>
      *> Longer than the policy figure warns; longer than the
      *> statutory maximum is a breach that needs an authorised
      *> exception before the order can be issued, and it is reported
      *> in the payment practices extract.
      *>
      *> The AGREED terms are snapshot on the order (OR-TERMS-SNAPSHOT)
      *> and flow unchanged to the supplier invoice due date - PAYTERMC
      *> computes that date with TERMCALC so purchasing and the
      *> purchase ledger cannot disagree about it.
      *>
      *> CA-FUNCTION values
      *>   CHECK     test proposed terms, no side effect
      *>   APPLY     test and return the snapshot text and due date
      *>   DUEDATE   compute the invoice due date for agreed terms
      *>
      *> CA-RETURN-CODE 00 within policy, 04 longer than policy,
      *>   06 breach - authorised exception required, 08 rejected,
      *>   12 severe, 16 terms code not found
      *>
      *> REASON CODES
      *>   PTC0001 company or terms code missing
      *>   PTC0002 terms code not found
      *>   PTC0003 terms are longer than the company payment policy
      *>   PTC0004 terms breach the statutory maximum
      *>   PTC0005 an exception needs an approver and a reason
      *>   PTC0006 unsupported function
      *>   PTC0007 negative or impossible day count
      *>   PTC0008 the due date could not be calculated
      *>================================================================
      *> 2012-09-03 KMP  CR2001 LATE PAYMENT REGULATIONS
      *> 2017-04-06 KMP  CR2341 PAYMENT PRACTICES REPORTING
      *> 2026-10-06 STERP D-25 STATUTORY MAXIMUM AND EXCEPTIONS
      *>================================================================
      *> REQ: PO-027, D-25
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  PT-BODY.
               10  PT-SUPPLIER         PIC X(20).
               10  PT-ORDER-NUMBER     PIC X(30).
               10  PT-TERMS-CODE       PIC X(12).
               10  PT-ORDER-DATE       PIC 9(8).
               10  PT-INVOICE-DATE     PIC 9(8).
               10  PT-VALUE-GBP        PIC S9(13)V99.
               10  PT-SUPPLIER-SIZE    PIC X(10).
                   88  PT-SIZE-LARGE             VALUE "LARGE".
                   88  PT-SIZE-SME               VALUE "SME".
                   88  PT-SIZE-MICRO             VALUE "MICRO".
               10  PT-LIMIT-SOURCE     PIC X(10).
                   88  PT-LIMITS-PASSED          VALUE "PASSED".
                   88  PT-LIMITS-LOOKUP          VALUE "LOOKUP".
               10  PT-POLICY-DAYS-IN   PIC S9(5).
               10  PT-STAT-MAX-IN      PIC S9(5).
               10  PT-EXCEPTION-BY     PIC X(20).
               10  PT-EXCEPTION-REASON PIC X(300).
      *>     ---- answers ----
               10  PT-TERMS-DAYS       PIC S9(5).
               10  PT-TERMS-BASIS      PIC X(20).
               10  PT-SETT-DISC-PCT    PIC S9(5)V9(4).
               10  PT-SETT-DISC-DAYS   PIC S9(5).
               10  PT-TERMS-SNAPSHOT   PIC X(100).
               10  PT-DUE-DATE         PIC 9(8).
               10  PT-POLICY-DAYS      PIC S9(5).
               10  PT-STAT-MAX-DAYS    PIC S9(5).
               10  PT-EFFECTIVE-LIMIT  PIC S9(5).
               10  PT-EXCESS-DAYS      PIC S9(5).
               10  PT-REPORTABLE       PIC X.
                   88  PT-IN-PRACTICES           VALUE "Y".
               10  PT-NEEDS-EXCEPTION  PIC X.
                   88  PT-EXCEPTION-REQD         VALUE "Y".
               10  PT-APPROVAL-ID      PIC S9(17).
               10  PT-OUTCOME          PIC X(10).
                   88  PT-WITHIN                 VALUE "WITHIN".
                   88  PT-WARNED                 VALUE "WARN".
                   88  PT-BREACH                 VALUE "BREACH".
                   88  PT-EXCEPTED               VALUE "EXCEPTED".
