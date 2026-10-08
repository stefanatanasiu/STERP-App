      *>================================================================
      *> PERIODCKCA - COMMAREA for PERIODCK (posting date policy)
      *> CA-FUNCTION values
      *>   CHECK     validate PC-POSTING-DATE for CA-COMPANY through
      *>             fin.usp_ValidatePeriod, for PC-ACTION (POST,
      *>             REVERSE, ADJUST ...)
      *>   DATES     check that the business event date, the tax point
      *>             and the posting date are each valid and report the
      *>             period of the posting date
      *> COM-009 - date validation distinguishes business event dates
      *> from posting and tax dates and enforces the open period
      *> policy.  Backdating a document does not authorise posting into
      *> a closed period: the period status is returned so that the
      *> caller can offer the authorised route (D-15 - only the
      *> financial controller reopens a period).
      *> RETURN CODES 00 posting allowed, 06 blocked but an authorised
      *>   route exists (SOFT close), 08 blocked (HARD, FUTURE, no
      *>   period, invalid date), 12 SQL failure.
      *>================================================================
      *> REQ: COM-009
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  PC-BODY.
               10  PC-POSTING-DATE     PIC 9(8).
               10  PC-DOCUMENT-DATE    PIC 9(8).
               10  PC-TAX-POINT        PIC 9(8).
               10  PC-ACTION           PIC X(12).
               10  PC-FISCAL-YEAR      PIC S9(4) SIGN LEADING SEPARATE.
               10  PC-PERIOD-NO        PIC S9(4) SIGN LEADING SEPARATE.
               10  PC-PERIOD-YYYYPP    PIC X(6).
               10  PC-PERIOD-STATUS    PIC X(10).
               10  PC-ALLOWED          PIC X.
               10  PC-OVERRIDE-PERM    PIC X(40).
               10  PC-REASON-TEXT      PIC X(200).
