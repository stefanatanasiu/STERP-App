      *>================================================================
      *> TERMCALCCA - COMMAREA for TERMCALC (payment terms service)
      *> CA-FUNCTION values
      *>   DUEDATE   resolve terms and produce TC-DUE-DATE and, where
      *>             a settlement discount applies, TC-DISCOUNT-DATE
      *>             and TC-DISCOUNT-AMT
      *>   GETTERMS  read core.PaymentTerms into the TC-BASIS-* fields
      *>             without calculating a date
      *>   DISCOUNT  settlement discount amount only, from TC-NET-AMOUNT
      *>             / TC-VAT-AMOUNT and TC-DISCOUNT-PCT
      *> Resolution order for DUEDATE
      *>   1  TC-TERMS-CODE is present  -> read core.PaymentTerms
      *>   2  otherwise TC-BASIS-TYPE is present -> use it as supplied
      *>   3  otherwise the Late Payment of Commercial Debts (Interest)
      *>      Act 1998 default: 30 days after the later of supply and
      *>      the customer receiving notice of the amount (COM-012)
      *> TC-BASIS-TYPE
      *>   INVOICE         TC-DAYS from the invoice date
      *>   EOM             end of the invoice month plus TC-DAYS
      *>   DAYOFNEXTMONTH  TC-DAY-OF-MONTH of the following month; a day
      *>                   that does not exist becomes the month end
      *>   STATUTORY       Late Payment Act default (30 days)
      *> TC-BACS-ADJUST "Y" moves a due date that is not a processing
      *>   day in TC-BACS-DIRECTION (NEXT / PREV) on TC-CALENDAR; the
      *>   unadjusted date stays in TC-DUE-DATE-RAW (COM-026).
      *> RETURN CODES 00 ok, 04 warning (terms over the D-25 limit, due
      *>   date moved for Bacs, terms code not found and the statutory
      *>   default used), 08 invalid input (TRM0001..TRM0009).
      *>================================================================
      *> REQ: COM-012, COM-026   AC: AC-005
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  TC-BODY.
               10  TC-TERMS-CODE       PIC X(10).
               10  TC-BASIS-TYPE       PIC X(16).
               10  TC-DAYS             PIC S9(4) SIGN LEADING SEPARATE.
               10  TC-DAY-OF-MONTH     PIC S9(4) SIGN LEADING SEPARATE.
               10  TC-DISCOUNT-DAYS    PIC S9(4) SIGN LEADING SEPARATE.
               10  TC-DISCOUNT-PCT     PIC S9(5)V9(4) COMP-3.
               10  TC-INVOICE-DATE     PIC 9(8).
               10  TC-SUPPLY-DATE      PIC 9(8).
               10  TC-RECEIVED-DATE    PIC 9(8).
               10  TC-DUE-DATE         PIC 9(8).
               10  TC-DUE-DATE-RAW     PIC 9(8).
               10  TC-DISCOUNT-DATE    PIC 9(8).
               10  TC-NET-AMOUNT       PIC S9(13)V9(6) COMP-3.
               10  TC-VAT-AMOUNT       PIC S9(13)V9(6) COMP-3.
               10  TC-DISCOUNT-AMT     PIC S9(13)V9(6) COMP-3.
               10  TC-DISCOUNT-BASIS   PIC X(8).
               10  TC-BACS-ADJUST      PIC X.
               10  TC-BACS-DIRECTION   PIC X(4).
               10  TC-CALENDAR         PIC X(10).
               10  TC-TERM-DAYS        PIC S9(4) SIGN LEADING SEPARATE.
               10  TC-MAX-TERM-DAYS    PIC S9(4) SIGN LEADING SEPARATE.
               10  TC-LONG-TERM-FLAG   PIC X.
               10  TC-SOURCE           PIC X(10).
               10  TC-TERMS-DESC       PIC X(40).
               10  TC-DUE-DATE-UK      PIC X(10).
               10  TC-DISC-DATE-UK     PIC X(10).
